package org.mustbe.consulo.javascript.lang.parsing;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageVersion;
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
	public ASTNode parse(@NotNull IElementType root, @NotNull PsiBuilder builder, @NotNull LanguageVersion languageVersion)
	{
		JavaScriptParsingContext parsingContext = createParsingContext();

		final PsiBuilder.Marker rootMarker = builder.mark();
		while(!builder.eof())
		{
			parsingContext.getStatementParsing().parseSourceElement(builder);
		}
		rootMarker.done(root);
		return builder.getTreeBuilt();
	}

	@NotNull
	public JavaScriptParsingContext createParsingContext()
	{
		return new JavaScriptParsingContext();
	}
}
