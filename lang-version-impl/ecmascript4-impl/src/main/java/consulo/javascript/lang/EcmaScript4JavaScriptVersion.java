package consulo.javascript.lang;

import org.jetbrains.annotations.NotNull;
import consulo.javascript.lang.parsing.EcmaScript4Parser;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingFlexLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import consulo.annotations.DeprecationInfo;

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

	@NotNull
	@Override
	public PsiParser createParser()
	{
		return new EcmaScript4Parser();
	}

	@NotNull
	@Override
	public JSHighlighter getSyntaxHighlighter()
	{
		return new JSHighlighter(myDialectOptionHolder);
	}

	@NotNull
	@Override
	public Lexer createLexer()
	{
		return new JavaScriptParsingFlexLexer(myDialectOptionHolder);
	}
}
