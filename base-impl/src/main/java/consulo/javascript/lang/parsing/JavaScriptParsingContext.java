package consulo.javascript.lang.parsing;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;

/**
 * @author VISTALL
 * @since 29.08.13.
 */
public class JavaScriptParsingContext {
    private final ExpressionParsing myExpressionParsing;
    private final StatementParsing myStatementParsing;
    private final FunctionParsing myFunctionParsing;

    public JavaScriptParsingContext() {
        myExpressionParsing = createExpressionParsing();
        myStatementParsing = createStatementParsing();
        myFunctionParsing = createFunctionParsing();
    }

    public boolean isIdentifierToken(PsiBuilder builder, IElementType tokenType) {
        if (isIdentifierName(builder, tokenType)) {
            return true;
        }

        if (JSTokenTypes.KEYWORDS.contains(tokenType)) {
            builder.remapCurrentToken(JSTokenTypes.IDENTIFIER);
            return true;
        }
        return false;
    }

    public boolean isIdentifierName(PsiBuilder builder, IElementType tokenType) {
        return JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(tokenType);
    }

    protected FunctionParsing createFunctionParsing() {
        return new FunctionParsing(this);
    }

    protected StatementParsing createStatementParsing() {
        return new StatementParsing(this);
    }

    protected ExpressionParsing createExpressionParsing() {
        return new ExpressionParsing(this);
    }

    public FunctionParsing getFunctionParsing() {
        return myFunctionParsing;
    }

    public ExpressionParsing getExpressionParsing() {
        return myExpressionParsing;
    }

    public StatementParsing getStatementParsing() {
        return myStatementParsing;
    }
}