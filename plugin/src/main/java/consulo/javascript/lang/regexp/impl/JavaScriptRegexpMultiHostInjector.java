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

package consulo.javascript.lang.regexp.impl;

import com.intellij.lang.injection.MultiHostInjector;
import com.intellij.lang.injection.MultiHostRegistrar;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiLanguageInjectionHost;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.psi.impl.JSRegExpLiteralExpressionImpl;
import org.intellij.lang.regexp.RegExpLanguage;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 11.12.2015
 */
public class JavaScriptRegexpMultiHostInjector implements MultiHostInjector
{
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
