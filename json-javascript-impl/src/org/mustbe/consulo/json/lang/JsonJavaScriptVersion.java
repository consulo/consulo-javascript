package org.mustbe.consulo.json.lang;

import consulo.lombok.annotations.Lazy;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import org.mustbe.consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import org.mustbe.consulo.json.lang.lexer.JsonLexer;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Factory;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JsonJavaScriptVersion extends BaseJavaScriptLanguageVersion
{
	private static final Factory<Lexer> ourLexerFactory = new Factory<Lexer>()
	{
		@Override
		public Lexer create()
		{
			return new JsonLexer();
		}
	};

	@NotNull
	@Lazy
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
	public SyntaxHighlighter getSyntaxHighlighter()
	{
		return new JavaScriptHighlighter(ourLexerFactory);
	}

	@NotNull
	@Override
	public Lexer createLexer(@Nullable Project project)
	{
		return ourLexerFactory.create();
	}

	@NotNull
	@Override
	public PsiParser createParser(@Nullable Project project)
	{
		return new JsonJavaScriptParser();
	}}
