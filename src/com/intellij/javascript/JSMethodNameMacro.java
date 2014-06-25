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

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.template.Expression;
import com.intellij.codeInsight.template.ExpressionContext;
import com.intellij.codeInsight.template.Macro;
import com.intellij.codeInsight.template.Result;
import com.intellij.codeInsight.template.TextResult;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;

public class JSMethodNameMacro extends Macro
{

	@Override
	@NonNls
	public String getName()
	{
		return "jsMethodName";
	}

	@Override
	public String getPresentableName()
	{
		return JSBundle.message("js.methodname.macro.description");
	}


	@Override
	@NonNls
	public String getDefaultValue()
	{
		return "";
	}

	@Override
	public Result calculateResult(@NotNull Expression[] params, ExpressionContext context)
	{
		final PsiElement elementAtCaret = JSClassNameMacro.findElementAtCaret(context);
		if(elementAtCaret != null)
		{
			JSFunction function = PsiTreeUtil.getParentOfType(elementAtCaret, JSFunction.class);
			if(function instanceof JSFunctionExpression)
			{
				function = ((JSFunctionExpression) function).getFunction();
			}

			if(function != null)
			{
				final String name = function.getName();
				if(name != null)
				{
					return new TextResult(name);
				}
			}
		}
		return null;
	}

	@Override
	public Result calculateQuickResult(@NotNull Expression[] params, ExpressionContext context)
	{
		return null;
	}

	@Override
	public LookupElement[] calculateLookupItems(@NotNull Expression[] params, ExpressionContext context)
	{
		return null;
	}
}
