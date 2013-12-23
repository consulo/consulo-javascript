package com.intellij.lang.javascript.formatter;

import org.jetbrains.annotations.NotNull;
import com.intellij.openapi.options.Configurable;
import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CodeStyleSettingsProvider;
import com.intellij.psi.codeStyle.CustomCodeStyleSettings;

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

	@Override
	@NotNull
	public Configurable createSettingsPage(final CodeStyleSettings settings, final CodeStyleSettings originalSettings)
	{
		return new JSCodeStyleConfigurable(settings, originalSettings);
	}
}
