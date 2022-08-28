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

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import consulo.javascript.language.JavaScriptBundle;
import consulo.language.editor.completion.lookup.LookupElement;
import consulo.language.editor.template.Expression;
import consulo.language.editor.template.ExpressionContext;
import consulo.language.editor.template.Result;
import consulo.language.editor.template.TextResult;
import consulo.language.editor.template.macro.Macro;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

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
		return JavaScriptBundle.message("js.methodname.macro.description");
	}


	@Override
	@NonNls
	public String getDefaultValue()
	{
		return "";
	}

	@Override
	public Result calculateResult(@Nonnull Expression[] params, ExpressionContext context)
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
	public Result calculateQuickResult(@Nonnull Expression[] params, ExpressionContext context)
	{
		return null;
	}

	@Override
	public LookupElement[] calculateLookupItems(@Nonnull Expression[] params, ExpressionContext context)
	{
		return null;
	}
}
