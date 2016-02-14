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

package org.mustbe.consulo.javascript.lang;

import org.intellij.lang.regexp.RegExpLanguage;
import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import com.intellij.lang.injection.MultiHostInjector;
import com.intellij.lang.injection.MultiHostRegistrar;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSSimpleLiteralExpression;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiLanguageInjectionHost;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 11.12.2015
 */
public class JavaScriptRegexpMultiHostInjector implements MultiHostInjector
{
	@Override
	@RequiredReadAction
	public void injectLanguages(@NotNull MultiHostRegistrar registrar, @NotNull PsiElement context)
	{
		if(context instanceof JSSimpleLiteralExpression)
		{
			IElementType literalElementType = ((JSSimpleLiteralExpression) context).getLiteralElementType();
			if(literalElementType == JSTokenTypes.REGEXP_LITERAL)
			{
				int textLength = context.getTextLength() - 1;
				String text = context.getText();

				if(text.charAt(textLength) != '/')
				{
					textLength --;
				}
				registrar.startInjecting(RegExpLanguage.INSTANCE).addPlace(null, null, (PsiLanguageInjectionHost) context, new TextRange(1, textLength)).doneInjecting();
			}
		}
	}
}
