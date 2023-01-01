package consulo.javascript.lang.parsing;

import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;
import consulo.language.parser.PsiParser;
import consulo.language.version.LanguageVersion;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class JavaScriptParser implements PsiParser
{
	@Nonnull
	@Override
	public ASTNode parse(@Nonnull IElementType root, @Nonnull PsiBuilder originalBuilder, @Nonnull LanguageVersion languageVersion)
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

	@Nonnull
	public JavaScriptParserBuilder createBuilder(PsiBuilder builder)
	{
		return new JavaScriptParserBuilder(builder);
	}

	@Nonnull
	public JavaScriptParsingContext createParsingContext()
	{
		return new JavaScriptParsingContext();
	}
}
