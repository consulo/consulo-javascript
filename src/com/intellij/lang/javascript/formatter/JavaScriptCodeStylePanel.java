package com.intellij.lang.javascript.formatter;

import com.intellij.application.options.TabbedLanguageCodeStylePanel;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.psi.codeStyle.CodeStyleSettings;

/**
 * @author VISTALL
 * @since 16.02.2015
 */
public class JavaScriptCodeStylePanel extends TabbedLanguageCodeStylePanel
{
	public JavaScriptCodeStylePanel(CodeStyleSettings currentSettings, CodeStyleSettings cloneSettings)
	{
		super(JavascriptLanguage.INSTANCE, currentSettings, cloneSettings);
	}

	@Override
	protected void initTabs(CodeStyleSettings settings)
	{
		super.initTabs(settings);
		addTab(new JSCodeStylePanel(settings));
	}
}
