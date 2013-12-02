package com.intellij.lang.javascript.formatter;

import com.intellij.psi.codeStyle.CodeStyleSettings;
import com.intellij.psi.codeStyle.CustomCodeStyleSettings;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 12, 2008
 *         Time: 10:25:17 PM
 */
public class JSCodeStyleSettings extends CustomCodeStyleSettings
{
	public int INDENT_PACKAGE_CHILDREN = DO_NOT_INDENT;
	public boolean USE_SEMICOLON_AFTER_STATEMENT = true;
	public String FIELD_PREFIX = "_";
	public String PROPERTY_PREFIX = "";

	public static final int DO_NOT_INDENT = 0;
	public static final int INDENT = 1;

	protected JSCodeStyleSettings(CodeStyleSettings container)
	{
		super("JSCodeStyleSettings", container);
	}
}
