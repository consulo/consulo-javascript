package com.intellij.lang.javascript;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.lang.Language;

/**
 * @by Maxim.Mossienko
 */
public abstract class JSLanguageDialect extends Language
{
	public JSLanguageDialect(@NonNls @NotNull String id)
	{
		super(JavaScriptSupportLoader.JAVASCRIPT.getLanguage(), id);
	}

	public abstract
	@NonNls
	String getFileExtension();
}
