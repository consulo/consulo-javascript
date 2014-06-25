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

/*
 * User: anna
 * Date: 01-Feb-2008
 */
package com.intellij.javascript;

import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.hint.ImplementationTextSelectioner;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;

public class JSImplementationTextSelectioner implements ImplementationTextSelectioner
{
	@Override
	public int getTextStartOffset(@NotNull PsiElement element)
	{
		return element.getTextOffset();
	}

	@Override
	public int getTextEndOffset(@NotNull PsiElement element)
	{
		if(element instanceof JSNamedElementProxy)
		{
			element = ((JSNamedElementProxy) element).getElement();
		}
		if(element instanceof JSDefinitionExpression)
		{
			element = PsiTreeUtil.getParentOfType(element, JSExpressionStatement.class);
		}
		return element.getTextRange().getEndOffset();
	}
}