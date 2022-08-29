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

package consulo.javascript.impl.lang.regexp.impl;

import consulo.annotation.component.ExtensionImpl;
import consulo.document.util.TextRange;
import consulo.language.inject.MultiHostRegistrar;
import consulo.language.psi.PsiElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.psi.impl.JSRegExpLiteralExpressionImpl;
import consulo.language.inject.MultiHostInjector;
import consulo.language.psi.PsiLanguageInjectionHost;
import org.intellij.lang.regexp.RegExpLanguage;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 11.12.2015
 */
@ExtensionImpl
public class JavaScriptRegexpMultiHostInjector implements MultiHostInjector
{
	@Nonnull
	@Override
	public Class<? extends PsiElement> getElementClass()
	{
		return JSRegExpLiteralExpressionImpl.class;
	}

	@Override
	@RequiredReadAction
	public void injectLanguages(@Nonnull MultiHostRegistrar registrar, @Nonnull PsiElement context)
	{
		if(context instanceof JSRegExpLiteralExpressionImpl)
		{
			String text = context.getText();

			// ignore flags for regexp
			int lastIndex = text.lastIndexOf('/');
			// empty regexp
			if(lastIndex == 0)
			{
				return;
			}

			registrar.startInjecting(RegExpLanguage.INSTANCE).addPlace(null, null, (PsiLanguageInjectionHost) context, new TextRange(1, lastIndex)).doneInjecting();
		}
	}
}
