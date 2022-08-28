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

import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.ImplementationTextSelectioner;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;

import javax.annotation.Nonnull;

@ExtensionImpl
public class JSImplementationTextSelectioner implements ImplementationTextSelectioner
{
	@RequiredReadAction
	@Override
	public int getTextStartOffset(@Nonnull PsiElement element)
	{
		return element.getTextOffset();
	}

	@RequiredReadAction
	@Override
	public int getTextEndOffset(@Nonnull PsiElement element)
	{
		if(element instanceof JSDefinitionExpression)
		{
			element = PsiTreeUtil.getParentOfType(element, JSExpressionStatement.class);
		}
		return element.getTextRange().getEndOffset();
	}

	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}
}