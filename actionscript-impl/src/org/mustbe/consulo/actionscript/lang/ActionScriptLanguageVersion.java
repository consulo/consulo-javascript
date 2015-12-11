package org.mustbe.consulo.actionscript.lang;

import org.consulo.lombok.annotations.LazyInstance;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import org.mustbe.consulo.javascript.lang.parsing.EcmaScript4Parser;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;

/**
 * @author VISTALL
 * @since 06.04.2015
 */
public class ActionScriptLanguageVersion extends BaseJavaScriptLanguageVersion
{
	@NotNull
	@LazyInstance
	public static ActionScriptLanguageVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(ActionScriptLanguageVersion.class);
	}

	private final DialectOptionHolder myDialectOptionHolder = new DialectOptionHolder(true, false);

	public ActionScriptLanguageVersion()
	{
		super("ActionScript");
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
