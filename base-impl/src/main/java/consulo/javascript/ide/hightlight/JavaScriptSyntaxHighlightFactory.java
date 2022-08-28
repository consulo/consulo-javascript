package consulo.javascript.ide.hightlight;

import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.language.editor.highlight.LanguageVersionableSyntaxHighlighterFactory;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.language.version.LanguageVersion;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public abstract class JavaScriptSyntaxHighlightFactory extends LanguageVersionableSyntaxHighlighterFactory
{
	public JavaScriptSyntaxHighlightFactory()
	{
	}

	@Nonnull
	@Override
	public SyntaxHighlighter getSyntaxHighlighter(@Nonnull LanguageVersion languageVersion)
	{
		if(languageVersion instanceof BaseJavaScriptLanguageVersion)
		{
			return ((BaseJavaScriptLanguageVersion) languageVersion).getSyntaxHighlighter();
		}
		throw new IllegalArgumentException(languageVersion.toString());
	}
}
