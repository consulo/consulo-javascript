package consulo.javascript.lang;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.lang.parsing.JavaScriptParser;
import consulo.javascript.language.JavaScriptFeature;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.language.JavaScriptLanguageVersion;
import consulo.language.ast.TokenSet;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.language.parser.PsiParser;

import jakarta.annotation.Nonnull;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public abstract class BaseJavaScriptLanguageVersion extends JavaScriptLanguageVersion
{
	private static TokenSet ourLiterals = TokenSet.orSet(JavaScriptTokenSets.STRING_LITERALS, TokenSet.create(JSTokenTypes.NUMERIC_LITERAL));

	private Set<JavaScriptFeature> myFeatures = new LinkedHashSet<>();

	public BaseJavaScriptLanguageVersion(String name, String... mimeTypes)
	{
		super(name, name, JavaScriptLanguage.INSTANCE, mimeTypes);
	}

	protected void addFeature(@Nonnull JavaScriptFeature feature)
	{
		myFeatures.add(feature);
	}

	@Nonnull
	public Set<JavaScriptFeature> getFeatures()
	{
		return myFeatures;
	}

	@Nonnull
	public abstract SyntaxHighlighter getSyntaxHighlighter();

	@Nonnull
	@Override
	public PsiParser createParser()
	{
		return new JavaScriptParser();
	}

	@Nonnull
	@Override
	public TokenSet getWhitespaceTokens()
	{
		return JavaScriptTokenSets.WHITE_SPACES;
	}

	@Nonnull
	@Override
	public TokenSet getCommentTokens()
	{
		return JSTokenTypes.COMMENTS;
	}

	@Nonnull
	@Override
	public TokenSet getStringLiteralElements()
	{
		return ourLiterals;
	}
}
