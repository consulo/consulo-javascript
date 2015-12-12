package org.mustbe.consulo.javascript.lang;

import org.consulo.lombok.annotations.LazyInstance;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JavaScript15LanguageVersion extends BaseJavaScriptLanguageVersion implements StandardJavaScriptVersions.Marker
{
	@NotNull
	@LazyInstance
	public static JavaScript15LanguageVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(JavaScript15LanguageVersion.class);
	}

	public JavaScript15LanguageVersion()
	{
		super("JAVASCRIPT_1_5");
	}

	@NotNull
	@Override
	public String getPresentableName()
	{
		return "JavaScript 1.5";
	}
	@NotNull
	@Override
	public Lexer createLexer(@Nullable Project project)
	{
		return new JavaScriptParsingLexer(DialectOptionHolder.dummy());
	}

	@NotNull
	@Override
	public JSHighlighter getSyntaxHighlighter()
	{
		return new JSHighlighter(DialectOptionHolder.dummy());
	}

	@Override
	public int getWeight()
	{
		return 0;
	}
}
