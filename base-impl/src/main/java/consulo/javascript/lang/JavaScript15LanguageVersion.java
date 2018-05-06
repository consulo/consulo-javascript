package consulo.javascript.lang;

import javax.annotation.Nonnull;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.util.Factory;
import consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import consulo.javascript.lang.lexer.JavaScript15Lexer;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JavaScript15LanguageVersion extends BaseJavaScriptLanguageVersion implements StandardJavaScriptVersions.Marker
{
	private static final Factory<Lexer> ourLexerFactory = () -> new JavaScript15Lexer();

	@Nonnull
	public static JavaScript15LanguageVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(JavaScript15LanguageVersion.class);
	}

	public JavaScript15LanguageVersion()
	{
		super("JAVASCRIPT_1_5");
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "JavaScript 1.5";
	}
	@Nonnull
	@Override
	public Lexer createLexer()
	{
		return ourLexerFactory.create();
	}

	@Nonnull
	@Override
	public SyntaxHighlighter getSyntaxHighlighter()
	{
		return new JavaScriptHighlighter(ourLexerFactory);
	}

	@Override
	public int getWeight()
	{
		return 0;
	}
}
