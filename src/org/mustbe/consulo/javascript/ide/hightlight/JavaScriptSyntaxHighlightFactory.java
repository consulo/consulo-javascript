package org.mustbe.consulo.javascript.ide.hightlight;

import org.consulo.fileTypes.LanguageVersionableSyntaxHighlighterFactory;
import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JavaScriptSyntaxHighlightFactory extends LanguageVersionableSyntaxHighlighterFactory
{
	public JavaScriptSyntaxHighlightFactory()
	{
		super(JavascriptLanguage.INSTANCE);
	}

	@NotNull
	@Override
	public SyntaxHighlighter getSyntaxHighlighter(@NotNull LanguageVersion languageVersion)
	{
		if(languageVersion instanceof BaseJavaScriptLanguageVersion)
		{
			return ((BaseJavaScriptLanguageVersion) languageVersion).getSyntaxHighlighter();
		}
		throw new IllegalArgumentException(languageVersion.toString());
	}
}
