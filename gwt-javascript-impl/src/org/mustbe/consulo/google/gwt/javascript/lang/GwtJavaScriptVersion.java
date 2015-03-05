package org.mustbe.consulo.google.gwt.javascript.lang;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.google.gwt.javascript.ide.highlight.GwtSyntaxHighlighter;
import org.mustbe.consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class GwtJavaScriptVersion extends BaseJavaScriptLanguageVersion
{
	private final DialectOptionHolder myDialectOptionHolder = new DialectOptionHolder(false, true);

	public GwtJavaScriptVersion()
	{
		super("GWT");
	}

	@NotNull
	@Override
	public Lexer createLexer(@Nullable Project project)
	{
		return new JavaScriptParsingLexer(myDialectOptionHolder);
	}

	@NotNull
	@Override
	public JSHighlighter getSyntaxHighlighter()
	{
		return new GwtSyntaxHighlighter(myDialectOptionHolder);
	}
}
