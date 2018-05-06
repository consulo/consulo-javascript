package consulo.json.lang;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.util.Factory;
import consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.json.JsonFileType;
import consulo.json.lang.lexer.JsonLexer;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JsonJavaScriptVersion extends BaseJavaScriptLanguageVersion
{
	private static final Factory<Lexer> ourLexerFactory = JsonLexer::new;

	@Nonnull
	public static JsonJavaScriptVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(JsonJavaScriptVersion.class);
	}

	public JsonJavaScriptVersion()
	{
		super("JSON", "application/json");
	}

	@Nullable
	@Override
	public FileType getAssociatedFileType()
	{
		return JsonFileType.INSTANCE;
	}

	@Nonnull
	@Override
	public SyntaxHighlighter getSyntaxHighlighter()
	{
		return new JavaScriptHighlighter(ourLexerFactory);
	}

	@Nonnull
	@Override
	public Lexer createLexer()
	{
		return ourLexerFactory.create();
	}

	@Nonnull
	@Override
	public PsiParser createParser()
	{
		return new JsonJavaScriptParser();
	}}
