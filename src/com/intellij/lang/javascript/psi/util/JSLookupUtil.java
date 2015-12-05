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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredDispatchThread;
import com.intellij.codeInsight.completion.PrioritizedLookupElement;
import com.intellij.codeInsight.completion.util.ParenthesesInsertHandler;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.ide.IconDescriptorUpdaters;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
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
	@RequiredDispatchThread
	public static LookupElement createLookupItem(@NotNull PsiElement value, @NotNull String name, @NotNull LookupPriority priority)
	{
		LookupElementBuilder builder = LookupElementBuilder.create(name);
		builder = builder.withTypeText(value.getContainingFile().getName(), true);
		builder = builder.withIcon(IconDescriptorUpdaters.getIcon(value, 0));
		if(value instanceof JSFunction)
		{
			JSParameterList parameterList = ((JSFunction) value).getParameterList();
			JSParameter[] jsParameters = parameterList == null ? JSParameter.EMPTY_ARRAY : parameterList.getParameters();
			builder = builder.withPresentableText(name + "(" + StringUtil.join(jsParameters, new Function<JSParameter, String>()
			{
				@Override
				public String fun(JSParameter jsParameter)
				{
					return jsParameter.getName();
				}
			}, ", ") + ")");
			builder = builder.withInsertHandler(ParenthesesInsertHandler.getInstance(jsParameters.length > 0));
		}

		if(priority == LookupPriority.NORMAL)
		{
			return builder;
		}
		return PrioritizedLookupElement.withPriority(builder, priority.ordinal());
	}
}
