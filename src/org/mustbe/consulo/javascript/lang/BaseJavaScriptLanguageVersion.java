package org.mustbe.consulo.javascript.lang;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.parsing.JavaScriptParser;
import com.intellij.lang.BaseLanguageVersion;
import com.intellij.lang.LanguageVersionWithParsing;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.openapi.project.Project;
import com.intellij.psi.tree.TokenSet;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public abstract class BaseJavaScriptLanguageVersion extends BaseLanguageVersion<JavascriptLanguage> implements
		LanguageVersionWithParsing<JavascriptLanguage>
{
	public BaseJavaScriptLanguageVersion(String name)
	{
		super(name, JavascriptLanguage.INSTANCE);
	}

	@NotNull
	public abstract JSHighlighter getSyntaxHighlighter();

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
		return JSTokenSets.WHITE_SPACES;
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
		return JSTokenSets.STRING_LITERALS;
	}
}
