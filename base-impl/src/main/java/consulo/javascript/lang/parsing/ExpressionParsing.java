/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package consulo.javascript.lang.parsing;

import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.parser.PsiBuilder;
import consulo.localize.LocalizeValue;
import consulo.logging.Logger;
import consulo.util.dataholder.Key;

import jakarta.annotation.Nullable;


/**
 * @author max
 * @since 2005-01-28
 */
public class ExpressionParsing<C extends JavaScriptParsingContext> extends Parsing<C> {
    private static final Logger LOG = Logger.getInstance(ExpressionParsing.class);

    public static final Key<Boolean> WITHIN_ARRAY_LITERAL_EXPRESSION = Key.create("within.array.literal.expression");
    public static final Key<Boolean> WITHIN_OBJECT_LITERAL_EXPRESSION = Key.create("within.object.literal.expression");

    private static final TokenSet DOT_SET = TokenSet.create(JSTokenTypes.DOT);
    private static final TokenSet MEMBER_OPERATOR_SET =
        TokenSet.create(JSTokenTypes.DOT, JSTokenTypes.COLON_COLON, JSTokenTypes.DOT_DOT, JSTokenTypes.QUEST_DOT);
    private static final TokenSet LITERAL_SET = TokenSet.create(
        JSTokenTypes.NUMERIC_LITERAL,
        JSTokenTypes.STRING_LITERAL,
        JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL,
        JSTokenTypes.INTERPOLATION_STRING_LITERAL,
        JSTokenTypes.NULL_KEYWORD,
        JSTokenTypes.UNDEFINED_KEYWORD,
        JSTokenTypes.FALSE_KEYWORD,
        JSTokenTypes.TRUE_KEYWORD
    );

    private final JSXParser myJSXParser = new JSXParser();

    public ExpressionParsing(C context) {
        super(context);
    }

    protected boolean parsePrimaryExpression(PsiBuilder builder) {
        IElementType firstToken = builder.getTokenType();
        if (firstToken == JSTokenTypes.THIS_KEYWORD) {
            Parsing.buildTokenElement(JSElementTypes.THIS_EXPRESSION, builder);
            return true;
        }
        else if (firstToken == JSTokenTypes.SUPER_KEYWORD) {
            Parsing.buildTokenElement(JSElementTypes.SUPER_EXPRESSION, builder);
            return true;
        }
        else if (isIdentifierName(builder, firstToken) || firstToken == JSTokenTypes.ANY_IDENTIFIER) {
            Parsing.buildTokenElement(JSElementTypes.REFERENCE_EXPRESSION, builder);
            return true;
        }
        else if (firstToken == JSTokenTypes.REGEXP_LITERAL) {
            LocalizeValue errorMessage = validateLiteral(builder);
            Parsing.buildTokenElement(JSElementTypes.REGEXP_LITERAL_EXPRESSION, builder);
            if (errorMessage != null) {
                builder.error(errorMessage);
            }
            return true;
        }
        else if (LITERAL_SET.contains(firstToken)) {
            LocalizeValue errorMessage = validateLiteral(builder);
            Parsing.buildTokenElement(JSElementTypes.LITERAL_EXPRESSION, builder);
            if (errorMessage != null) {
                builder.error(errorMessage);
            }
            return true;
        }
        else if (firstToken == JSTokenTypes.LPAR) {
            parseParenthesizedExpression(builder);
            return true;
        }
        else if (firstToken == JSTokenTypes.LBRACKET) {
            parseArrayLiteralExpression(builder);
            return true;
        }
        else if (firstToken == JSTokenTypes.LBRACE) {
            parseObjectLiteralExpression(builder);
            return true;
        }
        else if (firstToken == JSTokenTypes.LET_KEYWORD) {
            parseLetExpression(builder);
            return true;
        }
        else if (firstToken == JSTokenTypes.FUNCTION_KEYWORD) {
            getFunctionParsing().parseFunctionExpression(builder);
            return true;
        }
        else if (firstToken == JSTokenTypes.XML_START_TAG_START || firstToken == JSTokenTypes.XML_START_TAG_LIST) {
            parseTag(builder);
            return true;
        }
        else if (firstToken == JSTokenTypes.AT) {
            PsiBuilder.Marker marker = builder.mark();
            builder.advanceLexer();

            if (!builder.eof()) {
                IElementType tokenType = builder.getTokenType();
                if (tokenType == JSTokenTypes.ANY_IDENTIFIER || isIdentifierToken(builder, tokenType)) {
                    builder.advanceLexer();
                }
                else if (tokenType == JSTokenTypes.LBRACKET) {
                    builder.advanceLexer();
                    parseExpression(builder);
                    Parsing.checkMatches(builder, JSTokenTypes.RBRACKET, JavaScriptLocalize.javascriptParserMessageExpectedRbracket());
                }
                else {
                    builder.error(JavaScriptLocalize.javascriptParserMessageExpectedIdentifier());
                }
            }

            marker.done(JSElementTypes.REFERENCE_EXPRESSION);

            return true;
        }
        else {
            return false;
        }
    }

    private void parseLetExpression(final PsiBuilder builder) {
        final PsiBuilder.Marker marker = getStatementParsing().parseLetExpressionStart(builder);
        parseExpression(builder);
        marker.done(JSElementTypes.LET_EXPRESSION);
    }

    private void parseTag(final PsiBuilder builder) {
        myJSXParser.setBuilder(builder);

        myJSXParser.parseTag(false);
    }

    @Nullable
    public static LocalizeValue validateLiteral(final PsiBuilder builder) {
        final IElementType ttype = builder.getTokenType();
        if (ttype == JSTokenTypes.STRING_LITERAL || ttype == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL) {
            final String ttext = builder.getTokenText();
            assert ttext != null;

            if (lastSymbolEscaped(ttext) ||
                ttext.startsWith("\"") && (!ttext.endsWith("\"") || ttext.length() == 1) ||
                ttext.startsWith("\'") && (!ttext.endsWith("\'") || ttext.length() == 1)) {
                return JavaScriptLocalize.javascriptParserMessageUnclosedStringLiteral();
            }
        }

        return null;
    }

    private static boolean lastSymbolEscaped(String text) {
        boolean escapes = false;
        boolean escaped = true;
        for (int i = 0; i < text.length(); i++) {
            char c = text.charAt(i);
            if (escapes) {
                escapes = false;
                escaped = true;
                continue;
            }
            if (c == '\\') {
                escapes = true;
            }
            escaped = false;
        }
        return escapes || escaped;
    }

    public void parseObjectLiteralExpression(final PsiBuilder builder) {
        ExpressionParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.LBRACE);
        final PsiBuilder.Marker expr = builder.mark();
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
                builder.error(JavaScriptLocalize.javascriptParserPropertyExpected());
            }
            else if (isNotPropertyStart(builder, elementType)) {
                break;
            }
        }

        Parsing.checkMatches(builder, JSTokenTypes.RBRACE, JavaScriptLocalize.javascriptParserMessageExpectedRbrace());
        expr.done(JSElementTypes.OBJECT_LITERAL_EXPRESSION);
    }

    public boolean isNotPropertyStart(PsiBuilder builder, IElementType elementType) {
        return !isIdentifierToken(builder, elementType)
            && !JavaScriptTokenSets.STRING_LITERALS.contains(elementType)
            && elementType != JSTokenTypes.NUMERIC_LITERAL;
    }

    protected void parseProperty(final PsiBuilder builder) {
        final IElementType nameToken = builder.getTokenType();
        final PsiBuilder.Marker property = builder.mark();

        if (isNotPropertyStart(builder, nameToken)) {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedIdentifierStringLiteralOrNumericLiteral());
        }
        builder.advanceLexer();

        Parsing.checkMatches(builder, JSTokenTypes.COLON, JavaScriptLocalize.javascriptParserMessageExpectedColon());

        builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, Boolean.TRUE);
        if (!parseAssignmentExpression(builder)) {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
        }
        builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, null);

        property.done(JSElementTypes.PROPERTY);
    }

    public void parseArrayLiteralExpression(final PsiBuilder builder) {
        ExpressionParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.LBRACKET);
        final PsiBuilder.Marker expr = builder.mark();
        builder.advanceLexer();
        boolean commaExpected = false;

        Boolean save = null;

        try {
            save = builder.getUserData(WITHIN_ARRAY_LITERAL_EXPRESSION);
            builder.putUserData(WITHIN_ARRAY_LITERAL_EXPRESSION, Boolean.TRUE);

            while (builder.getTokenType() != JSTokenTypes.RBRACKET) {
                if (commaExpected) {
                    final boolean b =
                        Parsing.checkMatches(builder, JSTokenTypes.COMMA, JavaScriptLocalize.javascriptParserMessageExpectedComma());
                    if (!b) {
                        break;
                    }
                }

                while (builder.getTokenType() == JSTokenTypes.COMMA) {
                    builder.advanceLexer();
                }

                commaExpected = false;
                if (builder.getTokenType() != JSTokenTypes.RBRACKET) {
                    if (!parseAssignmentExpression(builder)) {
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
        finally {
            builder.putUserData(WITHIN_ARRAY_LITERAL_EXPRESSION, save);
        }
    }

    private void parseParenthesizedExpression(PsiBuilder builder) {
        ExpressionParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.LPAR);
        PsiBuilder.Marker expr = builder.mark();
        builder.advanceLexer();
        parseExpression(builder);
        Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());
        expr.done(JSElementTypes.PARENTHESIZED_EXPRESSION);
    }

    protected boolean parseMemberExpression(PsiBuilder builder, boolean allowCallSyntax) {
        return parseMemberExpression(builder, allowCallSyntax, MEMBER_OPERATOR_SET);
    }

    protected boolean parseMemberExpression(PsiBuilder builder, boolean allowCallSyntax, TokenSet membersOperatorSet) {
        PsiBuilder.Marker expr = builder.mark();
        boolean isNew;

        IElementType type = builder.getTokenType();

        if (type == JSTokenTypes.NEW_KEYWORD) {
            isNew = true;
            boolean isfunction = parseNewExpression(builder);

            if (isfunction) {
                expr.done(JSElementTypes.NEW_EXPRESSION);
                if (builder.getTokenType() != JSTokenTypes.LPAR) {
                    return true;
                }
                expr = expr.precede();
                isNew = false;
            }
        }
        else {
            isNew = false;
            if (!parsePrimaryExpression(builder)) {
                expr.drop();
                return false;
            }
        }

        boolean currentlyAllowCallSyntax = allowCallSyntax && (type != JSTokenTypes.LBRACE && type != JSTokenTypes.LBRACKET);

        while (true) {
            IElementType tokenType = builder.getTokenType();
            if (membersOperatorSet.contains(tokenType)) {
                currentlyAllowCallSyntax = allowCallSyntax;
                builder.advanceLexer();

                if (builder.getTokenType() == JSTokenTypes.AT) {
                    builder.advanceLexer();
                }

                tokenType = builder.getTokenType();

                if (tokenType == JSTokenTypes.LBRACKET || tokenType == JSTokenTypes.LPAR) {
                    continue;
                }

                if (tokenType == JSTokenTypes.ANY_IDENTIFIER || isIdentifierToken(builder, tokenType)) {
                    builder.advanceLexer();
                }
                else {
                    builder.error(JavaScriptLocalize.javascriptParserMessageExpectedName());
                }

                expr.done(JSElementTypes.REFERENCE_EXPRESSION);
                expr = expr.precede();
            }
            else if (tokenType == JSTokenTypes.LBRACKET) {
                builder.advanceLexer();
                parseExpression(builder);
                Parsing.checkMatches(builder, JSTokenTypes.RBRACKET, JavaScriptLocalize.javascriptParserMessageExpectedRbracket());
                expr.done(JSElementTypes.INDEXED_PROPERTY_ACCESS_EXPRESSION);
                expr = expr.precede();
            }
            else if (currentlyAllowCallSyntax && tokenType == JSTokenTypes.LPAR) {
                parseArgumentList(builder);
                expr.done(isNew ? JSElementTypes.NEW_EXPRESSION : JSElementTypes.CALL_EXPRESSION);
                expr = expr.precede();
                isNew = false;
            }
            else {
                if (isNew) {
                    expr.done(JSElementTypes.NEW_EXPRESSION);
                }
                else {
                    expr.drop();
                }
                break;
            }
        }

        return true;
    }

    public boolean parseQualifiedTypeName(PsiBuilder builder) {
        return parseQualifiedTypeName(builder, false);
    }

    public boolean parseQualifiedTypeName(PsiBuilder builder, boolean allowStar) {
        return parseQualifiedTypeName(builder, allowStar, DOT_SET);
    }

    public boolean parseQualifiedTypeName(PsiBuilder builder, boolean allowStar, TokenSet separatorsSet) {
        if (JSTokenTypes.IDENTIFIER != builder.getTokenType()) {
            return false;
        }
        PsiBuilder.Marker expr = builder.mark();
        Parsing.buildTokenElement(JSElementTypes.REFERENCE_EXPRESSION, builder);

        while (separatorsSet.contains(builder.getTokenType())) {
            boolean stop = false;
            builder.advanceLexer();

            final IElementType tokenType = builder.getTokenType();
            if (tokenType == JSTokenTypes.ANY_IDENTIFIER && allowStar) {
                builder.advanceLexer();
                stop = true;
            }
            else if (tokenType == JSTokenTypes.DEFAULT_KEYWORD) {
                builder.advanceLexer(); // TODO: allow any keyword
            }
            else {
                Parsing.checkMatches(builder, JSTokenTypes.IDENTIFIER, JavaScriptLocalize.javascriptParserMessageExpectedName());
            }
            expr.done(JSElementTypes.REFERENCE_EXPRESSION);
            expr = expr.precede();

            if (stop) {
                break;
            }
        }

        expr.drop();
        return true;
    }

    protected boolean parseNewExpression(PsiBuilder builder) {
        ExpressionParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.NEW_KEYWORD);
        builder.advanceLexer();

        if (builder.getTokenType() == JSTokenTypes.FUNCTION_KEYWORD) {
            getFunctionParsing().parseFunctionExpression(builder);
            return true;
        }

        if (!parseMemberExpression(builder, false)) {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
        }
        return false;
    }

    protected void parseArgumentList(final PsiBuilder builder) {
        ExpressionParsing.LOG.assertTrue(builder.getTokenType() == JSTokenTypes.LPAR);
        final PsiBuilder.Marker arglist = builder.mark();
        builder.advanceLexer();
        boolean first = true;
        while (builder.getTokenType() != JSTokenTypes.RPAR) {
            if (first) {
                first = false;
            }
            else if (builder.getTokenType() == JSTokenTypes.COMMA) {
                builder.advanceLexer();
            }
            else {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedCommaOrRparen());
                break;
            }
            if (!parseAssignmentExpression(builder)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
        }

        Parsing.checkMatches(builder, JSTokenTypes.RPAR, JavaScriptLocalize.javascriptParserMessageExpectedRparen());
        arglist.done(JSElementTypes.ARGUMENT_LIST);
    }

    public void parseExpression(PsiBuilder builder) {
        if (!parseExpressionOptional(builder)) {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
        }
    }

    public boolean parseAssignmentExpressionNoIn(final PsiBuilder builder) {
        return parseAssignmentExpression(builder, false);
    }

    public boolean parseAssignmentExpression(final PsiBuilder builder) {
        return parseAssignmentExpression(builder, true);
    }

    private boolean parseAssignmentExpression(final PsiBuilder builder, boolean allowIn) {
        final PsiBuilder.Marker expr = builder.mark();

        if (JSTokenTypes.ASSIGNMENT_OPERATIONS.contains(builder.getTokenType()) && builder.getUserData(WITHIN_OBJECT_LITERAL_EXPRESSION) == null) {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            builder.advanceLexer();
            if (!parseAssignmentExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.ASSIGNMENT_EXPRESSION);
            return true;
        }

        final PsiBuilder.Marker definitionExpr = builder.mark();
        if (!parseConditionalExpression(builder, allowIn)) {
            definitionExpr.drop();
            expr.drop();
            return false;
        }

        if (JSTokenTypes.ASSIGNMENT_OPERATIONS.contains(builder.getTokenType())) {
            definitionExpr.done(JSElementTypes.DEFINITION_EXPRESSION);
            builder.advanceLexer();
            if (!parseAssignmentExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.ASSIGNMENT_EXPRESSION);
        }
        else {
            definitionExpr.drop();
            expr.drop();
        }
        return true;
    }

    private boolean parseConditionalExpression(final PsiBuilder builder, final boolean allowIn) {
        final PsiBuilder.Marker expr = builder.mark();
        if (!parseORExpression(builder, allowIn)) {
            expr.drop();
            return false;
        }

        final IElementType nextTokenType = builder.getTokenType();

        if (nextTokenType == JSTokenTypes.QUEST) {
            builder.advanceLexer();
            if (!parseAssignmentExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            Parsing.checkMatches(builder, JSTokenTypes.COLON, JavaScriptLocalize.javascriptParserMessageExpectedColon());
            if (!parseAssignmentExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.CONDITIONAL_EXPRESSION);
        }
        else if (nextTokenType == JSTokenTypes.FOR_KEYWORD && builder.getUserData(WITHIN_ARRAY_LITERAL_EXPRESSION) != null) {
            getStatementParsing().parseForLoopHeader(builder);      // TODO: make it more clear
            expr.done(JSElementTypes.CONDITIONAL_EXPRESSION);
        }
        else {
            expr.drop();
        }
        return true;
    }

    private boolean parseORExpression(final PsiBuilder builder, final boolean allowIn) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseANDExpression(builder, allowIn)) {
            expr.drop();
            return false;
        }

        while (builder.getTokenType() == JSTokenTypes.OROR) {
            builder.advanceLexer();
            if (!parseANDExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseANDExpression(final PsiBuilder builder, final boolean allowIn) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseBitwiseORExpression(builder, allowIn)) {
            expr.drop();
            return false;
        }

        while (builder.getTokenType() == JSTokenTypes.ANDAND) {
            builder.advanceLexer();
            if (!parseBitwiseORExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseBitwiseORExpression(final PsiBuilder builder, final boolean allowIn) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseBitwiseXORExpression(builder, allowIn)) {
            expr.drop();
            return false;
        }

        while (builder.getTokenType() == JSTokenTypes.OR) {
            builder.advanceLexer();
            if (!parseBitwiseXORExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseBitwiseXORExpression(final PsiBuilder builder, final boolean allowIn) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseBitwiseANDExpression(builder, allowIn)) {
            expr.drop();
            return false;
        }

        while (builder.getTokenType() == JSTokenTypes.XOR) {
            builder.advanceLexer();
            if (!parseBitwiseANDExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseBitwiseANDExpression(final PsiBuilder builder, final boolean allowIn) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseEqualityExpression(builder, allowIn)) {
            expr.drop();
            return false;
        }

        while (builder.getTokenType() == JSTokenTypes.AND) {
            builder.advanceLexer();
            if (!parseEqualityExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseEqualityExpression(final PsiBuilder builder, final boolean allowIn) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseRelationalExpression(builder, allowIn)) {
            expr.drop();
            return false;
        }

        while (JSTokenTypes.EQUALITY_OPERATIONS.contains(builder.getTokenType())) {
            builder.advanceLexer();
            if (!parseRelationalExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseRelationalExpression(final PsiBuilder builder, final boolean allowIn) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseShiftExpression(builder)) {
            expr.drop();
            return false;
        }
        while (JSTokenTypes.RELATIONAL_OPERATIONS.contains(builder.getTokenType())
            && (allowIn || builder.getTokenType() != JSTokenTypes.IN_KEYWORD)) {
            builder.advanceLexer();
            if (!parseShiftExpression(builder)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseShiftExpression(final PsiBuilder builder) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseAdditiveExpression(builder)) {
            expr.drop();
            return false;
        }
        while (JSTokenTypes.SHIFT_OPERATIONS.contains(builder.getTokenType())) {
            builder.advanceLexer();
            if (!parseAdditiveExpression(builder)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseAdditiveExpression(final PsiBuilder builder) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseMultiplicativeExpression(builder)) {
            expr.drop();
            return false;
        }
        while (JSTokenTypes.ADDITIVE_OPERATIONS.contains(builder.getTokenType())) {
            builder.advanceLexer();
            if (!parseMultiplicativeExpression(builder)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseIsAsExpression(final PsiBuilder builder) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseUnaryExpression(builder)) {
            expr.drop();
            return false;
        }

        while (builder.getTokenType() == JSTokenTypes.AS_KEYWORD || builder.getTokenType() == JSTokenTypes.IS_KEYWORD) {
            builder.advanceLexer();
            if (!parseUnaryExpression(builder)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    protected boolean parseMultiplicativeExpression(final PsiBuilder builder) {
        return parseMultiplicativeExpression(builder, JSTokenTypes.MULTIPLICATIVE_OPERATIONS);
    }

    protected boolean parseMultiplicativeExpression(final PsiBuilder builder, TokenSet operatorSet) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseIsAsExpression(builder)) {
            expr.drop();
            return false;
        }

        while (operatorSet.contains(builder.getTokenType())) {
            builder.advanceLexer();
            if (!parseUnaryExpression(builder)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.BINARY_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();
        return true;
    }

    private boolean parseUnaryExpression(final PsiBuilder builder) {
        final IElementType tokenType = builder.getTokenType();
        if (JSTokenTypes.UNARY_OPERATIONS.contains(tokenType)) {
            final PsiBuilder.Marker expr = builder.mark();
            builder.advanceLexer();
            if (!parseUnaryExpression(builder)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.PREFIX_EXPRESSION);
            return true;
        }
        else {
            return parsePostfixExpression(builder);
        }
    }

    private boolean parsePostfixExpression(PsiBuilder builder) {
        final PsiBuilder.Marker expr = builder.mark();
        if (!parseMemberExpression(builder, true)) {
            expr.drop();
            return false;
        }

        final IElementType tokenType = builder.getTokenType();
        if (tokenType == JSTokenTypes.PLUSPLUS || tokenType == JSTokenTypes.MINUSMINUS) {
            builder.advanceLexer();
            expr.done(JSElementTypes.POSTFIX_EXPRESSION);
        }
        else {
            expr.drop();
        }
        return true;
    }

    public boolean parseExpressionOptional(final PsiBuilder builder) {
        return parseExpressionOptional(builder, true);
    }

    public boolean parseExpressionOptionalNoIn(final PsiBuilder builder) {
        return parseExpressionOptional(builder, false);
    }

    public boolean parseExpressionOptional(final PsiBuilder builder, final boolean allowIn) {
        PsiBuilder.Marker expr = builder.mark();
        if (!parseAssignmentExpression(builder, allowIn)) {
            expr.drop();
            return false;
        }

        if (builder.getTokenType() == JSTokenTypes.IN_KEYWORD) {
            expr.done(JSElementTypes.DEFINITION_EXPRESSION);
            return true;
        }

        while (builder.getTokenType() == JSTokenTypes.COMMA) {
            builder.advanceLexer();
            if (!parseAssignmentExpression(builder, allowIn)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }

            expr.done(JSElementTypes.COMMA_EXPRESSION);
            expr = expr.precede();
        }

        expr.drop();

        return true;
    }

    public boolean tryParseType(final PsiBuilder builder) {
        return false;
    }

    public boolean parseType(final PsiBuilder builder) {
        final IElementType tokenType = builder.getTokenType();
        if (JSDocumentationUtils.PRIMITIVE_TYPE_FILTER.contains(tokenType)) {
            builder.advanceLexer();
        }
        else if (!parseQualifiedTypeName(builder)) {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedTypename());
        }
        return true;
    }

    public boolean parseSimpleExpression(final PsiBuilder builder) {
        if (!parseUnaryExpression(builder)) {
            builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            return false;
        }
        return true;
    }
}
