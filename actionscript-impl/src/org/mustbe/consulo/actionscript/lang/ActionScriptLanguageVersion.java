package org.mustbe.consulo.actionscript.lang;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import org.mustbe.consulo.javascript.lang.parsing.EcmaScript4Parser;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingFlexLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import consulo.lombok.annotations.Lazy;

/**
 * @author VISTALL
 * @since 06.04.2015
 */
public class ActionScriptLanguageVersion extends BaseJavaScriptLanguageVersion
{
	@NotNull
	@Lazy
	public static ActionScriptLanguageVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(ActionScriptLanguageVersion.class);
	}

	private final DialectOptionHolder myDialectOptionHolder = new DialectOptionHolder(true, false);

	public ActionScriptLanguageVersion()
	{
		super("ACTIONSCRIPT");
	}

	@NotNull
	@Override
	public String getPresentableName()
	{
		return "ActionScript";
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
