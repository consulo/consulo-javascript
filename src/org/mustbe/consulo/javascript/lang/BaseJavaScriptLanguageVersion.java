package org.mustbe.consulo.javascript.lang;

import java.util.Set;

import org.consulo.annotations.Immutable;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.parsing.JavaScriptParser;
import com.intellij.lang.BaseLanguageVersion;
import com.intellij.lang.LanguageVersionWithParsing;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.project.Project;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.containers.ArrayListSet;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public abstract class BaseJavaScriptLanguageVersion extends BaseLanguageVersion<JavaScriptLanguage> implements LanguageVersionWithParsing<JavaScriptLanguage>
{
	private Set<JavaScriptFeature> myFeatures = new ArrayListSet<JavaScriptFeature>();

	public BaseJavaScriptLanguageVersion(String name)
	{
		super(name, JavaScriptLanguage.INSTANCE);
	}

	protected void addFeature(@NotNull JavaScriptFeature feature)
	{
		myFeatures.add(feature);
	}

	@NotNull
	@Immutable
	public Set<JavaScriptFeature> getFeatures()
	{
		return myFeatures;
	}

	@NotNull
	public String getPresentableName()
	{
		return getName();
	}

	@NotNull
	public abstract SyntaxHighlighter getSyntaxHighlighter();

	@NotNull
	@Override
	public PsiParser createParser(@Nullable Project project)
	{
		return new JavaScriptParser();
	}

	@NotNull
	@Override
	public TokenSet getWhitespaceTokens()
	{
		return JavaScriptTokenSets.WHITE_SPACES;
	}

	@NotNull
	@Override
	public TokenSet getCommentTokens()
	{
		return JSTokenTypes.COMMENTS;
	}

	@NotNull
	@Override
	public TokenSet getStringLiteralElements()
	{
		return JavaScriptTokenSets.STRING_LITERALS;
	}
}
