package org.mustbe.consulo.json.lang;

import org.consulo.lombok.annotations.LazyInstance;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JSONLexer;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JsonJavaScriptVersion extends BaseJavaScriptLanguageVersion
{
	@NotNull
	@LazyInstance
	public static JsonJavaScriptVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(JsonJavaScriptVersion.class);
	}

	public JsonJavaScriptVersion()
	{
		super("JSON");
	}

	@NotNull
	@Override
	public JSHighlighter getSyntaxHighlighter()
	{
		return new JSHighlighter(DialectOptionHolder.dummy())
		{
			@Override
			@NotNull
			public Lexer getHighlightingLexer()
			{
				return new JSONLexer(super.getHighlightingLexer());
			}
		};
	}

	@NotNull
	@Override
	public Lexer createLexer(@Nullable Project project)
	{
		return new JSONLexer(new JavaScriptParsingLexer(DialectOptionHolder.dummy()));
	}

	@NotNull
	@Override
	public PsiParser createParser(@Nullable Project project)
	{
		return new JsonJavaScriptParser();
	}}
