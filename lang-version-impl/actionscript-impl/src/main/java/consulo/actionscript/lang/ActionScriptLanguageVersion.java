package consulo.actionscript.lang;

import javax.annotation.Nonnull;

import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingFlexLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.javascript.lang.parsing.EcmaScript4Parser;

/**
 * @author VISTALL
 * @since 06.04.2015
 */
public class ActionScriptLanguageVersion extends BaseJavaScriptLanguageVersion
{
	@Nonnull
	public static ActionScriptLanguageVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(ActionScriptLanguageVersion.class);
	}

	private final DialectOptionHolder myDialectOptionHolder = new DialectOptionHolder(true, false);

	public ActionScriptLanguageVersion()
	{
		super("ACTIONSCRIPT");
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "ActionScript";
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
