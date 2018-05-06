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

package com.intellij.javascript;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import com.intellij.codeInsight.CodeInsightBundle;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.MutableLookupElement;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSSuperExpression;
import com.intellij.lang.parameterInfo.CreateParameterInfoContext;
import com.intellij.lang.parameterInfo.ParameterInfoContext;
import com.intellij.lang.parameterInfo.ParameterInfoHandlerWithTabActionSupport;
import com.intellij.lang.parameterInfo.ParameterInfoUIContext;
import com.intellij.lang.parameterInfo.ParameterInfoUtils;
import com.intellij.lang.parameterInfo.UpdateParameterInfoContext;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.search.searches.DefinitionsScopedSearch;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.ContainerUtil;

/**
 * @author Maxim.Mossienko
 */
public class JSParameterInfoHandler implements ParameterInfoHandlerWithTabActionSupport<JSArgumentList, JSFunction, JSExpression>
{
	private static final Set<Class> ourArgumentListAllowedParentClassesSet = ContainerUtil.<Class>newHashSet(JSCallExpression.class);

	@Override
	public boolean couldShowInLookup()
	{
		return true;
	}

	@Override
	public Object[] getParametersForLookup(final LookupElement item, final ParameterInfoContext context)
	{
		if(!(item instanceof MutableLookupElement))
		{
			return null;
		}

		PsiElement element = item.getPsiElement();
		if(element instanceof JSFunction)
		{
			final JSFunction originalFunction = (JSFunction) element;
			final List<JSFunction> lookupItems = new ArrayList<JSFunction>();
			Set<String> availableSignatures = new HashSet<String>();

			for(PsiElement el : DefinitionsScopedSearch.search(originalFunction))
			{
				doAddSignature(lookupItems, availableSignatures, el);
			}

			if(lookupItems.size() == 0)
			{
				lookupItems.add(originalFunction);
			}

			return lookupItems.toArray(new Object[lookupItems.size()]);
		}

		return ArrayUtil.EMPTY_OBJECT_ARRAY;
	}

	private static void doAddSignature(final List<JSFunction> lookupItems, final Set<String> availableSignatures, final PsiElement el)
	{
		if(el instanceof JSFunction)
		{
			final JSFunction function = (JSFunction) el;
			final JSParameterList parameterList = function.getParameterList();

			if(parameterList != null)
			{
				final String typedSignature = buildSignature(parameterList.getParameters(), false, -1).text;
				final String untypedSignature = buildSignature(parameterList.getParameters(), true, -1).text;

				if(!availableSignatures.contains(typedSignature) && !availableSignatures.contains(untypedSignature))
				{
					lookupItems.add(function);
					availableSignatures.add(typedSignature);
					availableSignatures.add(untypedSignature);
				}
			}
		}
	}

	@Override
	public Object[] getParametersForDocumentation(final JSFunction p, final ParameterInfoContext context)
	{
		final JSParameterList list = p.getParameterList();
		if(list != null)
		{
			return list.getParameters();
		}
		return ArrayUtil.EMPTY_OBJECT_ARRAY;
	}

	@Override
	public JSArgumentList findElementForParameterInfo(final CreateParameterInfoContext context)
	{
		JSArgumentList argList = findArgumentList(context.getFile(), context.getOffset());

		if(argList != null)
		{
			return fillSignaturesForArgumentList(context, argList);
		}
		return argList;
	}

	@Nullable
	public static JSArgumentList findArgumentList(final PsiFile file, final int offset)
	{
		JSArgumentList argList = ParameterInfoUtils.findParentOfType(file, offset, JSArgumentList.class);
		if(argList == null)
		{
			final JSCallExpression callExpression = ParameterInfoUtils.findParentOfType(file, offset, JSCallExpression.class);
			if(callExpression != null)
			{
				argList = callExpression.getArgumentList();
			}
		}
		return argList;
	}

	@Nullable
	private static JSArgumentList fillSignaturesForArgumentList(final CreateParameterInfoContext context, final @Nonnull JSArgumentList argList)
	{
		final PsiElement psiElement = argList.getParent();
		if(!(psiElement instanceof JSCallExpression))
		{
			return null;
		}

		final JSCallExpression parent = (JSCallExpression) psiElement;
		final JSExpression methodExpression = parent.getMethodExpression();

		if(methodExpression instanceof JSReferenceExpression)
		{
			final ResolveResult[] resolveResults = ((JSReferenceExpression) methodExpression).multiResolve(true);

			if(resolveResults.length > 0)
			{
				List<JSFunction> items = new ArrayList<JSFunction>(resolveResults.length);
				Set<String> availableSignatures = new HashSet<String>();

				for(ResolveResult r : resolveResults)
				{
					PsiElement element = r.getElement();
					if(element instanceof JSProperty)
					{
						element = ((JSProperty) element).getValue();
					}

					doAddSignature(items, availableSignatures, element);
				}

				context.setItemsToShow(ArrayUtil.toObjectArray(items));
				return argList;
			}
		}
		else if(methodExpression instanceof JSSuperExpression)
		{
			final PsiElement clazz = methodExpression.getReference().resolve();
			if(clazz instanceof JSFunction)
			{
				context.setItemsToShow(new Object[]{clazz});
				return argList;
			}
		}
		return null;
	}

	@Override
	public void showParameterInfo(@Nonnull final JSArgumentList element, final CreateParameterInfoContext context)
	{
		context.showHint(element, element.getTextOffset(), this);
	}

	@Override
	public JSArgumentList findElementForUpdatingParameterInfo(final UpdateParameterInfoContext context)
	{
		return findArgumentList(context.getFile(), context.getOffset());
	}

	@Override
	public void updateParameterInfo(@Nonnull final JSArgumentList o, final UpdateParameterInfoContext context)
	{
		if(context.getParameterOwner() != o)
		{
			context.removeHint();
			return;
		}
		final int currentParameterIndex = ParameterInfoUtils.getCurrentParameterIndex(o.getNode(), context.getOffset(), JSTokenTypes.COMMA);
		context.setCurrentParameter(currentParameterIndex);
	}

	@Override
	@Nonnull
	public String getParameterCloseChars()
	{
		return ",){";
	}

	@Override
	public boolean tracksParameterIndex()
	{
		return true;
	}

	@Override
	public void updateUI(final JSFunction p, final ParameterInfoUIContext context)
	{
		final JSParameterList parameterList = p.getParameterList();
		final JSParameter[] params = parameterList != null ? parameterList.getParameters() : new JSParameter[0];
		final int currentParameterIndex = context.getCurrentParameterIndex() >= 0 ? context.getCurrentParameterIndex() : params.length;
		final JSParameter parameter = currentParameterIndex < params.length ? params[currentParameterIndex] : null;

		final SignatureInfo signatureInfo = buildSignature(params, false, currentParameterIndex);
		final String name = signatureInfo.text;

		final String currentParameterSignature = parameter != null ? getSignatureForParameter(parameter, false) : null;
		int highlightStart = parameter != null ? signatureInfo.selectedParameterStart : 0;
		int highlightEnd = parameter != null ? highlightStart + currentParameterSignature.length() : 0;
		context.setupUIComponentPresentation(name, highlightStart, highlightEnd, false, false, false, context.getDefaultParameterColor());
	}

	private static class SignatureInfo
	{
		String text;
		int selectedParameterStart = -1;
	}

	private static
	@Nonnull
	SignatureInfo buildSignature(final JSParameter[] params, final boolean skipType, int selectedParameterIndex)
	{
		SignatureInfo info = new SignatureInfo();
		if(params.length > 0)
		{
			StringBuilder result = new StringBuilder();
			for(int i = 0; i < params.length; ++i)
			{
				if(result.length() > 0)
				{
					result.append(", ");
				}
				if(selectedParameterIndex == i)
				{
					info.selectedParameterStart = result.length();
				}
				result.append(getSignatureForParameter(params[i], skipType));
			}

			info.text = result.toString();
		}
		else
		{
			info.text = CodeInsightBundle.message("parameter.info.no.parameters");
		}
		return info;
	}

	public static String getSignatureForParameter(final JSParameter p, boolean skipType)
	{
		final String s = skipType ? null : p.getTypeString();

		if(s != null && s.length() > 0)
		{
			final boolean ecmal4 = p.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;
			String result;

			if(ecmal4)
			{
				if(p.isRest())
				{
					result = "...";
				}
				else
				{
					result = p.getName() + ":" + s;
				}
			}
			else
			{
				result = "[" + s + "] " + p.getName();
			}
			final String initializerText = p.getInitializerText();
			if(initializerText != null)
			{
				result += " = " + initializerText;
			}
			return result;
		}
		return p.getName();
	}

	@Override
	@Nonnull
	public JSExpression[] getActualParameters(@Nonnull final JSArgumentList jsArgumentList)
	{
		return jsArgumentList.getArguments();
	}

	@Override
	@Nonnull
	public IElementType getActualParameterDelimiterType()
	{
		return JSTokenTypes.COMMA;
	}

	@Override
	@Nonnull
	public IElementType getActualParametersRBraceType()
	{
		return JSTokenTypes.RBRACE;
	}

	@Override
	@Nonnull
	public Set<Class> getArgumentListAllowedParentClasses()
	{
		return ourArgumentListAllowedParentClassesSet;
	}

	@Nonnull
	@Override
	public Set<? extends Class> getArgListStopSearchClasses()
	{
		return Collections.emptySet();
	}

	@Override
	@Nonnull
	public Class<JSArgumentList> getArgumentListClass()
	{
		return JSArgumentList.class;
	}
}
