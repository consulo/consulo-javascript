package org.mustbe.consulo.javascript.lang;

import com.intellij.lang.Language;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class JavaScriptLanguage extends Language
{
	public static final JavaScriptLanguage INSTANCE = new JavaScriptLanguage();

	public JavaScriptLanguage()
	{
		super("JavaScript", "text/javascript", "application/javascript");
	}

	@Override
	public boolean isCaseSensitive()
	{
		return true;
	}
}
