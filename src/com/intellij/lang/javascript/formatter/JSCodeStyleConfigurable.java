package com.intellij.lang.javascript.formatter;

import com.intellij.application.options.CodeStyleAbstractConfigurable;
import com.intellij.application.options.CodeStyleAbstractPanel;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.psi.codeStyle.CodeStyleSettings;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Mar 12, 2008
 * Time: 10:36:16 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSCodeStyleConfigurable extends CodeStyleAbstractConfigurable
{
	public JSCodeStyleConfigurable(final CodeStyleSettings settings, final CodeStyleSettings codeStyleSettings)
	{
		super(settings, codeStyleSettings, JSBundle.message("js.code.style.tab.name"));
	}

	@Override
	protected CodeStyleAbstractPanel createPanel(final CodeStyleSettings settings)
	{
		return new JSCodeStylePanel(settings);
	}

	@Override
	public String getHelpTopic()
	{
		return null; // TODO
	}
}
