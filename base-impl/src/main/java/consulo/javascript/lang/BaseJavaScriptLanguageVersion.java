package consulo.javascript.lang;

import java.util.Set;

import javax.annotation.Nonnull;

import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.containers.ArrayListSet;
import consulo.javascript.lang.parsing.JavaScriptParser;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public abstract class BaseJavaScriptLanguageVersion extends JavaScriptLanguageVersion
{
	private static TokenSet ourLiterals = TokenSet.orSet(JavaScriptTokenSets.STRING_LITERALS, TokenSet.create(JSTokenTypes.NUMERIC_LITERAL));

	private Set<JavaScriptFeature> myFeatures = new ArrayListSet<>();

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
