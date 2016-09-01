package org.mustbe.consulo.javascript.lang.parsing;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import consulo.lang.LanguageVersion;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class JavaScriptParser implements PsiParser
{
	@NotNull
	@Override
	public ASTNode parse(@NotNull IElementType root, @NotNull PsiBuilder originalBuilder, @NotNull LanguageVersion languageVersion)
	{
		JavaScriptParsingContext parsingContext = createParsingContext();

		JavaScriptParserBuilder builder = createBuilder(originalBuilder);

		final PsiBuilder.Marker rootMarker = builder.mark();
		while(!builder.eof())
		{
			parsingContext.getStatementParsing().parseSourceElement(builder);
		}
		rootMarker.done(root);
		return builder.getTreeBuilt();
	}

	@NotNull
	public JavaScriptParserBuilder createBuilder(PsiBuilder builder)
	{
		return new JavaScriptParserBuilder(builder);
	}

	@NotNull
	public JavaScriptParsingContext createParsingContext()
	{
		return new JavaScriptParsingContext();
	}
}
