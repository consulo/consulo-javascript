package consulo.json.lang;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.lang.parsing.ExpressionParsing;
import consulo.javascript.lang.parsing.Parsing;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;
import consulo.language.parser.PsiParser;
import consulo.language.version.LanguageVersion;
import consulo.localize.LocalizeValue;
import consulo.logging.Logger;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 05.03.2015
 */
public class JsonJavaScriptParser implements PsiParser {
    private static final Logger LOG = Logger.getInstance(JsonJavaScriptParser.class);

    private int myPropertyDepth;

    @Nonnull
    @Override
    public ASTNode parse(@Nonnull IElementType root, @Nonnull PsiBuilder builder, @Nonnull LanguageVersion languageVersion) {
        PsiBuilder.Marker rootMarker = builder.mark();
        parseRoot(builder);
        rootMarker.done(root);
        return builder.getTreeBuilt();
    }

    private void parseRoot(PsiBuilder builder) {
        if (builder.getTokenType() == JSTokenTypes.LBRACKET) {
            parseArrayLiteralExpression(builder);
            if (builder.getTokenType() != null) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedEof());
            }
        }
        else if (builder.getTokenType() == JSTokenTypes.LBRACE) {
            parseObjectLiteralExpression(builder);
            if (builder.getTokenType() != null) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedEof());
            }
        }
        else {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedLbraceOrLbracket());
        }

        while (builder.getTokenType() != null) {
            builder.advanceLexer();
        }
    }

    private void parseProperty(PsiBuilder builder) {
        if (myPropertyDepth > 1000) {
            builder.error("Too big depth for property");
            int braceCount = 0;
            int bracketCount = 0;
            while (!builder.eof()) {
                IElementType tokenType = builder.getTokenType();
                if (tokenType == JSTokenTypes.LBRACE) {
                    braceCount++;
                }
                else if (tokenType == JSTokenTypes.LBRACKET) {
                    bracketCount++;
                }
                else if (tokenType == JSTokenTypes.RBRACE) {
                    braceCount--;
                    if (braceCount < 0) {
                        break;
                    }
                }
                else if (tokenType == JSTokenTypes.RBRACKET) {
                    bracketCount--;
                    if (bracketCount < 0) {
                        break;
                    }
                }
                builder.advanceLexer();
            }
        }

        IElementType nameToken = builder.getTokenType();
        PsiBuilder.Marker property = builder.mark();
        myPropertyDepth++;

        if (isNotPropertyStart(nameToken)) {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedIdentifierStringLiteralOrNumericLiteral());
        }
        builder.advanceLexer();
        Parsing.checkMatches(builder, JSTokenTypes.COLON, JavaScriptLocalize.javascriptParserMessageExpectedColon());

        if (!parseValue(builder)) {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
        }
        myPropertyDepth--;

        property.done(JSElementTypes.PROPERTY);
    }

    public void parseObjectLiteralExpression(PsiBuilder builder) {
        LOG.assertTrue(builder.getTokenType() == JSTokenTypes.LBRACE);
        PsiBuilder.Marker expr = builder.mark();
        builder.advanceLexer();

        IElementType elementType = builder.getTokenType();

        while (elementType != JSTokenTypes.RBRACE && elementType != null) {
            parseProperty(builder);

            elementType = builder.getTokenType();
            if (elementType == JSTokenTypes.RBRACE) {
                break;
            }
            else if (elementType == JSTokenTypes.COMMA) {
                builder.advanceLexer();

                if (builder.getTokenType() == JSTokenTypes.RBRACE) {
                    break;
                }
            }
            else {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedComma());
            }

            elementType = builder.getTokenType();
            if (elementType == JSTokenTypes.RBRACE) {
                break;
            }
            else if (isNotPropertyStart(elementType)) {
                break;
            }
        }

        Parsing.checkMatches(builder, JSTokenTypes.RBRACE, JavaScriptLocalize.javascriptParserMessageExpectedRbrace());
        expr.done(JSElementTypes.OBJECT_LITERAL_EXPRESSION);
    }

    public static boolean isNotPropertyStart(IElementType elementType) {
        return !JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(elementType)
            && !JavaScriptTokenSets.STRING_LITERALS.contains(elementType)
            && elementType != JSTokenTypes.NUMERIC_LITERAL;
    }

    public void parseArrayLiteralExpression(PsiBuilder builder) {
        JsonJavaScriptParser.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.LBRACKET);
        PsiBuilder.Marker expr = builder.mark();
        builder.advanceLexer();
        boolean commaExpected = false;

        while (builder.getTokenType() != JSTokenTypes.RBRACKET) {
            if (commaExpected) {
                boolean b = Parsing.checkMatches(builder, JSTokenTypes.COMMA, JavaScriptLocalize.javascriptParserMessageExpectedComma());
                if (!b) {
                    break;
                }
            }

            while (builder.getTokenType() == JSTokenTypes.COMMA) {
                builder.advanceLexer();
            }

            commaExpected = false;
            if (builder.getTokenType() != JSTokenTypes.RBRACKET) {
                if (!parseValue(builder)) {
                    builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
                    break;
                }
                else {
                    commaExpected = true;
                }
            }
        }
        Parsing.checkMatches(builder, JSTokenTypes.RBRACKET, JavaScriptLocalize.javascriptParserMessageExpectedRbracket());
        expr.done(JSElementTypes.ARRAY_LITERAL_EXPRESSION);
    }

    private boolean parseValue(PsiBuilder builder) {
        IElementType firstToken = builder.getTokenType();
        if (firstToken == JSTokenTypes.NUMERIC_LITERAL ||
            firstToken == JSTokenTypes.STRING_LITERAL ||
            firstToken == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL ||
            firstToken == JSTokenTypes.NULL_KEYWORD ||
            firstToken == JSTokenTypes.FALSE_KEYWORD ||
            firstToken == JSTokenTypes.TRUE_KEYWORD) {
            LocalizeValue errorMessage = ExpressionParsing.validateLiteral(builder);
            Parsing.buildTokenElement(JSElementTypes.LITERAL_EXPRESSION, builder);
            if (errorMessage != null) {
                builder.error(errorMessage);
            }
            return true;
        }
        if (firstToken == JSTokenTypes.LBRACKET) {
            parseArrayLiteralExpression(builder);
            return true;
        }
        else if (firstToken == JSTokenTypes.MINUS) {
            PsiBuilder.Marker marker = builder.mark();
            builder.advanceLexer();
            if (builder.getTokenType() == JSTokenTypes.NUMERIC_LITERAL) {
                builder.advanceLexer();
                marker.done(JSElementTypes.PREFIX_EXPRESSION);
            }
            else {
                marker.error("Unexpected token");
            }
            return true;
        }
        else if (firstToken == JSTokenTypes.LBRACE) {
            parseObjectLiteralExpression(builder);
            return true;
        }
        else {
            return false;
        }
    }
}
