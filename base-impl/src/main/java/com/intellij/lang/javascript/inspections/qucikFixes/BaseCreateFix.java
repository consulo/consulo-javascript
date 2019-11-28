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

package com.intellij.lang.javascript.inspections.qucikFixes;

import java.util.Collections;
import java.util.Set;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import com.intellij.codeInsight.CodeInsightUtilBase;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupItem;
import com.intellij.codeInsight.template.Expression;
import com.intellij.codeInsight.template.ExpressionContext;
import com.intellij.codeInsight.template.Result;
import com.intellij.codeInsight.template.Template;
import com.intellij.codeInsight.template.TemplateManager;
import com.intellij.codeInsight.template.TextResult;
import com.intellij.codeInsight.template.impl.MacroCallNode;
import com.intellij.codeInsight.template.macro.MacroFactory;
import com.intellij.codeInspection.LocalQuickFix;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.fileEditor.OpenFileDescriptor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiDocumentManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlAttributeValue;
import com.intellij.psi.xml.XmlFile;
import com.intellij.psi.xml.XmlTag;
import com.intellij.psi.xml.XmlText;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.JavaScriptFeature;
import consulo.javascript.lang.JavaScriptVersionUtil;
import consulo.javascript.lang.psi.JavaScriptType;

/**
 * @author Maxim.Mossienko
 */
public abstract class BaseCreateFix implements LocalQuickFix
{
	private static final String ANY_TYPE = "*";
	private static final String SCRIPT_TAG_NAME = "Script";

	@Override
	@RequiredReadAction
	public void applyFix(@Nonnull Project project, @Nonnull ProblemDescriptor descriptor)
	{
		final PsiElement psiElement = descriptor.getPsiElement();
		PsiFile file = psiElement.getContainingFile();
		PsiFile realFile = file.getContext() != null ? file.getContext().getContainingFile() : file;
		Set<JavaScriptFeature> features = JavaScriptVersionUtil.getFeatures(psiElement);

		JSReferenceExpression referenceExpression = (JSReferenceExpression) psiElement.getParent();
		final JSExpression qualifier = referenceExpression.getQualifier();
		PsiElement predefinedAnchor = null;

		boolean classFeature = features.contains(JavaScriptFeature.CLASS);
		if(qualifier != null && classFeature)
		{
			PsiElement type = getType(qualifier, file, features);
			if(type == null)
			{
				return; // can not resolve
			}

			if(type.isWritable())
			{
				if(type instanceof XmlBackedJSClassImpl)
				{
					type = type.getParent().getContainingFile();
				}
				final PsiElement element = JSResolveUtil.unwrapProxy(type);

				if(element instanceof JSClass)
				{
					file = type.getContainingFile();
					realFile = file;
					predefinedAnchor = element.getLastChild().getPrevSibling();
				}
				else if(element instanceof XmlFile)
				{
					file = type.getContainingFile();
					realFile = file;
					predefinedAnchor = element;
				}
			}
		}

		Editor editor = getEditor(project, realFile);
		if(editor == null)
		{
			return;
		}

		PsiElement anchor = predefinedAnchor != null ? predefinedAnchor : JSUtils.findStatementAnchor(referenceExpression, file);
		boolean insertAtEnd = false;
		PsiElement anchorParent = null;

		String prefix = "";
		String suffix = "";

		if(anchor != null && classFeature)
		{
			anchorParent = anchor.getParent();
			while(anchorParent != null && !(anchorParent instanceof JSClass) && !(anchorParent instanceof JSFile))
			{
				anchor = anchorParent;
				anchorParent = anchor.getParent();
			}

			insertAtEnd = anchorParent instanceof JSClass;

			XmlFile contextFile = null;
			if(anchorParent instanceof JSFile && anchorParent.getContext() != null)
			{
				final PsiElement context = anchorParent.getContext();
				if(context instanceof XmlAttributeValue || context instanceof XmlText && !(SCRIPT_TAG_NAME.equals(((XmlTag) context.getParent()).getLocalName())))
				{
					contextFile = (XmlFile) context.getContainingFile();
				}
			}
			else if(realFile instanceof XmlFile)
			{
				contextFile = (XmlFile) realFile;
			}

			if(contextFile != null)
			{
				final XmlTag rootTag = contextFile.getDocument().getRootTag();
				JSClass jsClass = XmlBackedJSClassImpl.getXmlBackedClass(rootTag);
				JSFile jsFile = ((XmlBackedJSClassImpl) jsClass).findFirstScriptTag();

				if(jsFile != null)
				{
					anchor = jsFile.getFirstChild();
					while(anchor instanceof PsiWhiteSpace || anchor instanceof PsiComment || anchor instanceof JSImportStatement)
					{
						PsiElement nextSibling = anchor.getNextSibling();
						if(nextSibling == null)
						{
							break;
						}
						anchor = nextSibling;
					}
				}
				else
				{
					jsFile = ((XmlBackedJSClassImpl) jsClass).createScriptTag();
					Document document = PsiDocumentManager.getInstance(file.getProject()).getDocument(contextFile.getContainingFile());
					PsiDocumentManager.getInstance(file.getProject()).doPostponedOperationsAndUnblockDocument(document);
					anchor = PsiTreeUtil.firstChild(jsFile.getContext());
					insertAtEnd = true;
				}
			}
		}

		if(anchor != null)
		{
			final TemplateManager templateManager = TemplateManager.getInstance(project);
			Template template = templateManager.createTemplate("", "");

			if(prefix.length() > 0)
			{
				template.addTextSegment(prefix);
			}
			if(insertAtEnd)
			{
				template.addTextSegment("\n");
			}
			template.setToReformat(true);

			boolean isStatic = false;
			if(classFeature)
			{
				if(qualifier != null)
				{
					if(qualifier instanceof JSReferenceExpression)
					{
						PsiElement qualifierResolve = ((JSReferenceExpression) qualifier).resolve();

						if(qualifierResolve instanceof JSClass || qualifierResolve instanceof XmlFile)
						{
							isStatic = true;
						}
					}
				}
				else
				{
					JSAttributeListOwner attributeListOwner = PsiTreeUtil.getNonStrictParentOfType(psiElement, JSAttributeListOwner.class);
					if(attributeListOwner instanceof JSVariable)
					{
						PsiElement grandParent = JSResolveUtil.findParent(attributeListOwner);

						if(!(grandParent instanceof JSFile) && !(grandParent instanceof JSClass))
						{
							attributeListOwner = PsiTreeUtil.getNonStrictParentOfType(grandParent, JSAttributeListOwner.class);
						}
					}
					if(attributeListOwner != null)
					{
						JSAttributeList attributeList = attributeListOwner.getAttributeList();
						if(attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.STATIC))
						{
							isStatic = true;
						}
					}
				}
			}

			int cdataStartOffset = 0, at;
			if(!insertAtEnd && anchor instanceof PsiWhiteSpace && (at = anchor.getText().indexOf("<![CDATA[")) != -1)
			{
				cdataStartOffset += at + "<!CDATA[".length() + 1;
				template.addTextSegment("\n");
			}

			buildTemplate(template, referenceExpression, features, isStatic, file, anchorParent);
			if(!insertAtEnd)
			{
				template.addTextSegment("\n");
			}

			if(suffix.length() > 0)
			{
				template.addTextSegment(suffix);
			}

			if(!insertAtEnd && anchor instanceof PsiWhiteSpace && anchor.textToCharArray()[0] == '\n' && anchor.getPrevSibling() instanceof PsiComment)
			{
				insertAtEnd = true;
			}

			final TextRange anchorRange = anchor.getTextRange();
			int offset = insertAtEnd ? anchorRange.getEndOffset() : anchorRange.getStartOffset();

			if(file != realFile || file instanceof XmlFile)
			{
				final PsiFile anchorContainingFile = anchor.getContainingFile();
				final PsiElement anchorFileContext = anchorContainingFile.getContext();

				if(anchorFileContext != null)
				{
					if(anchorFileContext instanceof XmlText)
					{
						if(cdataStartOffset != 0)
						{ //
							offset += cdataStartOffset;
						}
						else
						{
							offset += ((XmlText) anchorFileContext).displayToPhysical(0);
						}
					}
					offset += anchorFileContext.getTextOffset();
				}
			}
			editor.getCaretModel().moveToOffset(offset);
			templateManager.startTemplate(editor, template);
		}
	}

	public static
	@Nullable
	Editor getEditor(final Project project, final PsiFile realFile)
	{
		if(!CodeInsightUtilBase.getInstance().prepareFileForWrite(realFile))
		{
			return null;
		}

		return FileEditorManager.getInstance(project).openTextEditor(new OpenFileDescriptor(project, realFile.getVirtualFile(), 0), true);
	}

	@RequiredReadAction
	protected abstract void buildTemplate(final Template template,
			JSReferenceExpression referenceExpression,
			Set<JavaScriptFeature> features,
			boolean staticContext,
			PsiFile file,
			PsiElement anchorParent);

	private static String getTypeOfValue(final JSExpression passedParameterValue, final PsiFile file, Set<JavaScriptFeature> features)
	{
		final PsiElement type = getType(passedParameterValue, file, features);
		return type != null ? type instanceof JSClass ? ((JSClass) type).getQualifiedName() : ((PsiNamedElement) type).getName() : ANY_TYPE;
	}

	@RequiredReadAction
	static PsiElement getType(final JSExpression passedParameterValue, final PsiFile file, Set<JavaScriptFeature> features)
	{
		if(passedParameterValue instanceof JSReferenceExpression)
		{
			JavaScriptType type = passedParameterValue.getType();

			PsiElement targetElement = type.getTargetElement();
			if(targetElement instanceof JSClass)
			{
				return targetElement;
			}
		}
		return null;
	}

	protected static JSExpression addAccessModifier(final Template template, final JSReferenceExpression referenceExpression, final boolean ecma, boolean staticContext)
	{
		final JSExpression qualifier = referenceExpression.getQualifier();

		if(ecma)
		{
			if((qualifier == null || qualifier instanceof JSThisExpression))
			{
				template.addTextSegment("private ");
			}
			if(staticContext)
			{
				template.addTextSegment("static ");
			}
		}
		return qualifier;
	}

	private int uniqueCounter;

	protected void addCompletionVar(final Template template)
	{
		final Expression paramTypeExpr = new MacroCallNode(MacroFactory.createMacro("complete"));
		template.addVariable("__Type" + (uniqueCounter++), paramTypeExpr, paramTypeExpr, true);
	}

	protected void addSemicolonSegment(final Template template, final PsiFile file)
	{
		final String semicolon = JSChangeUtil.getSemicolon(file.getProject());
		if(semicolon.length() > 0)
		{
			template.addTextSegment(semicolon);
		}
	}

	public static void guessExprTypeAndAddSuchVariable(final JSExpression passedParameterValue, final Template template, final String var1, final PsiFile file, final Set<JavaScriptFeature> features)
	{
		String type = getTypeOfValue(passedParameterValue, file, features);

		if(ApplicationManager.getApplication().isUnitTestMode())
		{
			template.addTextSegment(type);
		}
		else
		{
			final MyExpression paramTypeExpr = new MyExpression(type);
			template.addVariable(var1 + "Type", paramTypeExpr, paramTypeExpr, true);
		}
	}

	protected void guessTypeAndAddTemplateVariable(Template template, JSExpression referenceExpression, PsiFile file)
	{
		String type = null;
		PsiElement elementForWhichExprTypeToEvaluate = null;
		PsiElement parent = referenceExpression.getParent();
		boolean isCall = false;

		if(parent instanceof JSCallExpression)
		{
			isCall = true;
			parent = parent.getParent();
		}

		if(parent instanceof JSDefinitionExpression)
		{
			PsiElement grandParent = parent.getParent();

			if(grandParent instanceof JSAssignmentExpression)
			{
				elementForWhichExprTypeToEvaluate = ((JSAssignmentExpression) grandParent).getROperand();
			}
		}
		else if(parent instanceof JSReturnStatement)
		{
			final JSFunction fun = PsiTreeUtil.getParentOfType(referenceExpression, JSFunction.class);

			if(fun != null)
			{
				final String typeString = fun.getReturnTypeString();

				if(typeString != null)
				{
					type = typeString;
				}
			}
		}
		else if(parent instanceof JSExpressionStatement && isCall)
		{
			type = "void";
		}
		else if(parent instanceof JSVariable)
		{
			type = ((JSVariable) parent).getTypeString();
		}
		else if(parent instanceof JSArgumentList)
		{
			JSParameter parameter = JSResolveUtil.findParameterForUsedArgument(isCall ? (JSExpression) referenceExpression.getParent() : referenceExpression, (JSArgumentList) parent);
			if(parameter != null)
			{
				type = parameter.getTypeString();
			}
		}
		else if(parent instanceof JSAssignmentExpression)
		{
			JSExpression lOperand = ((JSAssignmentExpression) parent).getLOperand();
			if(lOperand != null)
			{
				type = getTypeOfValue(lOperand, file, Collections.singleton(JavaScriptFeature.CLASS));
			}
		}

		String expressionType = elementForWhichExprTypeToEvaluate instanceof JSExpression ? JSResolveUtil.getExpressionType((JSExpression) elementForWhichExprTypeToEvaluate, file) : null;
		if(expressionType != null && !expressionType.equals("*"))
		{
			type = expressionType;
		}
		if(type == null)
		{
			addCompletionVar(template);
		}
		else
		{
			MyExpression expression = new MyExpression(type);
			template.addVariable("__type" + referenceExpression.getText(), expression, expression, true);
		}
	}

	@Nullable
	protected static JSClass findClass(PsiFile file, final PsiElement anchorParent)
	{
		if(anchorParent instanceof JSClass)
		{
			return (JSClass) anchorParent;
		}

		if(file instanceof JSFile)
		{
			return JSResolveUtil.getXmlBackedClass((JSFile) file);
		}
		return null;
	}

	public static class MyExpression extends Expression
	{
		TextResult result;
		private final String myVar1;

		public MyExpression(final String var1)
		{
			myVar1 = var1;
			result = new TextResult(myVar1);
		}

		@Override
		public Result calculateResult(ExpressionContext context)
		{
			return result;
		}

		@Override
		public Result calculateQuickResult(ExpressionContext context)
		{
			return result;
		}

		@Override
		public LookupElement[] calculateLookupItems(ExpressionContext context)
		{
			return LookupItem.EMPTY_ARRAY;
		}
	}
}
