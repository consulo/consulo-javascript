/*
 * Copyright 2013-2015 must-be.org
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

package com.intellij.lang.javascript.psi.util;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.psi.JavaScriptType;
import com.intellij.codeInsight.completion.PrioritizedLookupElement;
import com.intellij.codeInsight.completion.util.ParenthesesInsertHandler;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import consulo.ide.IconDescriptorUpdaters;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.openapi.util.Iconable;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.util.Function;

public class JSLookupUtil
{
	public static enum LookupPriority
	{
		NORMAL,
		HIGHER,
		HIGH,
		HIGHEST
	}

	@Nullable
	@RequiredReadAction
	public static LookupElement createLookupItem(@Nonnull PsiElement value, @Nonnull String name, @Nonnull LookupPriority priority)
	{
		LookupElementBuilder builder = LookupElementBuilder.create(name);
		builder = builder.withIcon(IconDescriptorUpdaters.getIcon(value, Iconable.ICON_FLAG_VISIBILITY));

		JSFunction function = value instanceof JSFunction ? (JSFunction) value : null;
		if(value instanceof JSProperty)
		{
			JSExpression expression = ((JSProperty) value).getValue();
			function = expression instanceof JSFunctionExpression ? ((JSFunctionExpression) expression).getFunction()  : null;
		}

		if(function != null)
		{
			JSParameterList parameterList = function.getParameterList();
			JSParameter[] jsParameters = parameterList == null ? JSParameter.EMPTY_ARRAY : parameterList.getParameters();
			builder = builder.withPresentableText(name + "(" + StringUtil.join(jsParameters, new Function<JSParameter, String>()
			{
				@Override
				public String fun(JSParameter jsParameter)
				{
					JavaScriptType type = jsParameter.getType();
					if(type != JavaScriptType.UNKNOWN)
					{
						return type.getPresentableText() + " " + jsParameter.getName();
					}
					return jsParameter.getName();
				}
			}, ", ") + ")");
			builder = builder.withInsertHandler(ParenthesesInsertHandler.getInstance(jsParameters.length > 0));
		}
		else if(value instanceof JSProperty)
		{
			JavaScriptType type = ((JSProperty) value).getType();
			if(type != JavaScriptType.UNKNOWN)
			{
				builder = builder.withTypeText(type.getPresentableText());
			}
		}
		else if(value instanceof JSVariable)
		{
			JavaScriptType type = ((JSVariable) value).getType();
			if(type != JavaScriptType.UNKNOWN)
			{
				builder = builder.withTypeText(type.getPresentableText());
			}
		}

		//builder = builder.withTailText(value.getContainingFile().getName(), true);
		if(priority == LookupPriority.NORMAL)
		{
			return builder;
		}
		return PrioritizedLookupElement.withPriority(builder, priority.ordinal());
	}
}
