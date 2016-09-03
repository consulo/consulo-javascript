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

package com.intellij.lang.javascript.formatter;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.Language;
import com.intellij.openapi.options.Configurable;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CodeStyleSettingsProvider;
import com.intellij.psi.codeStyle.CustomCodeStyleSettings;
import consulo.javascript.formatter.JavaScriptCodeStyleConfigurable;
import consulo.javascript.lang.JavaScriptLanguage;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 12, 2008
 *         Time: 10:32:33 PM
 */
public class JSCodeStyleSettingsProvider extends CodeStyleSettingsProvider
{
	@Override
	public CustomCodeStyleSettings createCustomSettings(final CodeStyleSettings settings)
	{
		return new JSCodeStyleSettings(settings);
	}

	@Nullable
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}

	@Override
	@NotNull
	public Configurable createSettingsPage(final CodeStyleSettings settings, final CodeStyleSettings originalSettings)
	{
		return new JavaScriptCodeStyleConfigurable(settings, originalSettings);
	}
}
