package consulo.javascript.ide.hightlight;

import javax.annotation.Nonnull;

import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import consulo.fileTypes.LanguageVersionableSyntaxHighlighterFactory;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.lang.LanguageVersion;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JavaScriptSyntaxHighlightFactory extends LanguageVersionableSyntaxHighlighterFactory
{
	public JavaScriptSyntaxHighlightFactory()
	{
		super(JavaScriptLanguage.INSTANCE);
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
