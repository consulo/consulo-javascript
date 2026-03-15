package consulo.javascript.lang.parsing;

import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;
import consulo.language.parser.PsiParser;
import consulo.language.version.LanguageVersion;


/**
 * @author VISTALL
 * @since 24.08.14
 */
public class JavaScriptParser implements PsiParser {
    @Override
    public ASTNode parse(IElementType root, PsiBuilder originalBuilder, LanguageVersion languageVersion) {
        JavaScriptParsingContext parsingContext = createParsingContext();

        JavaScriptParserBuilder builder = createBuilder(originalBuilder);

        PsiBuilder.Marker rootMarker = builder.mark();
        while (!builder.eof()) {
            parsingContext.getStatementParsing().parseSourceElement(builder);
        }
        rootMarker.done(root);
        return builder.getTreeBuilt();
    }

    public JavaScriptParserBuilder createBuilder(PsiBuilder builder) {
        return new JavaScriptParserBuilder(builder);
    }

    public JavaScriptParsingContext createParsingContext() {
        return new JavaScriptParsingContext();
    }
}
