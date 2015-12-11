package org.mustbe.consulo.javascript.lang;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.parsing.EcmaScript4Parser;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class EcmaScript4JavaScriptVersion extends BaseJavaScriptLanguageVersion
{
	private final DialectOptionHolder myDialectOptionHolder = new DialectOptionHolder(true, false);

	public EcmaScript4JavaScriptVersion()
	{
		super("ECMA4");
	}

	@NotNull
	@Override
	public PsiParser createParser(@Nullable Project project)
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
	public Lexer createLexer(@Nullable Project project)
	{
		return new JavaScriptParsingLexer(myDialectOptionHolder);
	}
}
