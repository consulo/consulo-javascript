package consulo.javascript.ecmascript4.lang;

import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingFlexLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import consulo.annotation.DeprecationInfo;
import consulo.javascript.ecmascript4.lang.parsing.EcmaScript4Parser;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.language.JavaScriptFeature;
import consulo.language.lexer.Lexer;
import consulo.language.parser.PsiParser;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
@Deprecated
@DeprecationInfo("This language version was dropped. We keep it only for history, many options may not supported")
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
