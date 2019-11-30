package consulo.javascript.lang;

import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingFlexLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import consulo.annotation.DeprecationInfo;
import consulo.javascript.lang.parsing.EcmaScript4Parser;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
@Deprecated
@DeprecationInfo("This language vesion was dropped. We keep it only for history, many options may not supported")
public class EcmaScript4JavaScriptVersion extends BaseJavaScriptLanguageVersion
{
	private final DialectOptionHolder myDialectOptionHolder = new DialectOptionHolder(true, false);

	public EcmaScript4JavaScriptVersion()
	{
		super("ECMA4");
		addFeature(JavaScriptFeature.CLASS);
	}

	@Nonnull
	@Override
	public PsiParser createParser()
	{
		return new EcmaScript4Parser();
	}

	@Nonnull
	@Override
	public JSHighlighter getSyntaxHighlighter()
	{
		return new JSHighlighter(myDialectOptionHolder);
	}

	@Nonnull
	@Override
	public Lexer createLexer()
	{
		return new JavaScriptParsingFlexLexer(myDialectOptionHolder);
	}
}
