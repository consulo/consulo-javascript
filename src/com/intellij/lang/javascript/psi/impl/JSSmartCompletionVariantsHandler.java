/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.psi.impl;

import gnu.trove.THashMap;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.psi.util.JSLookupUtil;
import com.intellij.lang.javascript.search.JSClassSearch;
import com.intellij.openapi.module.ModuleUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Ref;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.ResolveState;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlFile;
import com.intellij.psi.xml.XmlTag;
import com.intellij.util.Query;

/**
 * @author Maxim.Mossienko
 *         Date: Jan 25, 2008
 *         Time: 12:40:48 AM
 */
public class JSSmartCompletionVariantsHandler
{
	static Object[] getSmartVariants(final @NotNull PsiElement expr, final boolean ecma)
	{
		final PsiElement parent = expr.getParent();

		if(parent instanceof JSArgumentList &&
				((JSArgumentList) parent).getArguments()[0] == expr &&
				ecma &&
				((JSReferenceExpression) expr).getQualifier() == null)
		{
			final JSExpression calledExpr = ((JSCallExpression) parent.getParent()).getMethodExpression();

			if(calledExpr instanceof JSReferenceExpression)
			{
				final JSReferenceExpression expression = (JSReferenceExpression) calledExpr;
				final @NonNls String s = expression.getReferencedName();

				if("addEventListener".equals(s) || "removeEventListener".equals(s))
				{
					final List<Object> variants = new ArrayList<Object>();
					final MyEventSubclassesProcessor subclassesProcessor = new MyEventSubclassesProcessor(expr, variants);
					subclassesProcessor.findAcceptableVariants(expression, parent.getProject());
					if(variants.size() > 0)
					{
						return variants.toArray(new Object[variants.size()]);
					}
				}
			}
		}

		return null;
	}

	private static class MyEventSubclassesProcessor extends ResolveProcessor implements JSResolveUtil.MetaDataProcessor
	{
		private final JavaScriptIndex index;
		private final PsiElement myExpr;
		private final List<Object> myVariants;
		private final ResolveState state = new ResolveState();
		private final Map<String, JSVariable> myCandidatesMap = new THashMap<String, JSVariable>();
		private boolean findAcceptableEvents;

		public MyEventSubclassesProcessor(final PsiElement expr, final List<Object> variants)
		{
			super(null);
			myExpr = expr;
			myVariants = variants;
			index = JavaScriptIndex.getInstance(myExpr.getProject());

			setToProcessHierarchy(true);
		}

		public boolean process(final JSClass clazz)
		{
			clazz.processDeclarations(this, state, clazz, clazz);

			return true;
		}

		@Override
		public boolean execute(final PsiElement element, final ResolveState state)
		{
			if(element instanceof JSVariable)
			{
				final JSVariable variable = (JSVariable) element;
				final JSAttributeList attributeList = variable.getAttributeList();

				if(attributeList != null &&
						attributeList.getAccessType() == JSAttributeList.AccessType.PUBLIC &&
						attributeList.hasModifier(JSAttributeList.ModifierType.STATIC) &&
						"String".equals(variable.getTypeString()))
				{
					final String s = variable.getInitializerText();
					if(s != null && StringUtil.startsWith(s, "\"") && StringUtil.endsWith(s, "\""))
					{
						myCandidatesMap.put(StringUtil.stripQuotesAroundValue(s), variable);
					}
				}
			}

			if(findAcceptableEvents && element instanceof JSClass)
			{
				JSResolveUtil.processMetaAttributesForClass(element, this);
			}

			return true;
		}

		public void findAcceptableVariants(JSReferenceExpression expression, final Project project)
		{

			PsiElement clazz = JSResolveUtil.findClassByQName("flash.events.Event", index, ModuleUtil.findModuleForPsiElement(expression));
			clazz = JSResolveUtil.unwrapProxy(clazz);
			if(!(clazz instanceof JSClass))
			{
				return;
			}
			final Query<JSClass> query = JSClassSearch.searchClassInheritors((JSClass) clazz, true);

			for(JSClass extendedClass : query.findAll())
			{
				process(extendedClass);
			}

			final JSExpression qualifier = expression.getQualifier();

			JSClass clazzToProcess = null;

			if(qualifier instanceof JSThisExpression || qualifier instanceof JSSuperExpression)
			{
				clazzToProcess = PsiTreeUtil.getParentOfType(qualifier, JSClass.class);
			}
			else if(qualifier instanceof JSReferenceExpression)
			{
				final ResolveResult[] results = ((JSReferenceExpression) qualifier).multiResolve(false);
				if(results.length > 0 && results[0].getElement() instanceof JSClass)
				{
					clazzToProcess = (JSClass) results[0].getElement();
				}
			}

			if(clazzToProcess == null)
			{
				final PsiElement context = expression.getContainingFile().getContext();
				clazzToProcess = JSResolveUtil.getClassFromTagNameInMxml(context);
				if(clazzToProcess == null && context != null)
				{
					XmlFile file = PsiTreeUtil.getParentOfType(context, XmlFile.class);
					if(file != null)
					{
						final XmlTag rootTag = file.getDocument().getRootTag();
						final XmlTag[] tags = rootTag != null ? XmlBackedJSClassImpl.findMxmlSubTags(rootTag, "Metadata") : XmlTag.EMPTY;
						final MyJSInjectedFilesVisitor injectedFilesVisitor = new MyJSInjectedFilesVisitor();

						for(XmlTag tag : tags)
						{
							JSResolveUtil.processInjectedFileForTag(tag, injectedFilesVisitor);
						}
					}
				}
			}

			if(clazzToProcess != null)
			{
				findAcceptableEvents = true;
				setToProcessMembers(false);
				setTypeContext(true);

				clazzToProcess.processDeclarations(this, ResolveState.initial(), clazz, clazz);
			}
		}

		@Override
		public boolean process(final @NotNull JSAttribute jsAttribute)
		{
			if("Event".equals(jsAttribute.getName()))
			{
				final JSAttributeNameValuePair eventName = jsAttribute.getValueByName("name");

				if(eventName != null)
				{
					final String value = eventName.getSimpleValue();
					final JSVariable variable = myCandidatesMap.get(value);

					if(variable != null)
					{
						myCandidatesMap.remove(value);
						myVariants.add(JSLookupUtil.createPrioritizedLookupItem(variable, ((JSClass) variable.getParent().getParent()).getName() + "." + variable
								.getName(), 3));
					}
				}
			}
			return true;
		}

		@Override
		public boolean handleOtherElement(final PsiElement el, final PsiElement context, final Ref<PsiElement> continuePassElement)
		{
			return true;
		}

		private class MyJSInjectedFilesVisitor extends JSResolveUtil.JSInjectedFilesVisitor
		{
			@Override
			protected void process(final JSFile file)
			{
				for(PsiElement element : file.getChildren())
				{
					if(element instanceof JSAttributeList)
					{
						JSResolveUtil.processAttributeList(MyEventSubclassesProcessor.this, null, (JSAttributeList) element, true);
					}
				}
			}
		}
	}
}
