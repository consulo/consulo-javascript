/*
 * Copyright 2013-2016 must-be.org
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

package consulo.javascript.ecmascript.lang.parsing;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.ecmascript.psi.impl.EcmaScript6ElementTypes;
import consulo.javascript.lang.parsing.ExpressionParsing;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 03.03.2016
 */
public class EcmaScriptExpressionParsing extends ExpressionParsing<EcmaScriptParsingContext> {
    public EcmaScriptExpressionParsing(EcmaScriptParsingContext context) {
        super(context);
    }

    @Override
    public EcmaScriptStatementParsing getStatementParsing() {
        return (EcmaScriptStatementParsing)super.getStatementParsing();
    }

    @Override
    protected boolean parsePrimaryExpression(PsiBuilder builder) {
        if (builder.getTokenType() == JSTokenTypes.DOT_DOT_DOT) {
            PsiBuilder.Marker mark = builder.mark();
            builder.advanceLexer();

            if (!parseExpressionOptional(builder)) {
                mark.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            else {
                mark.done(JSElementTypes.SPREAD_EXPRESSION);
            }

            return true;
        }
        else if (builder.getTokenType() == JSTokenTypes.CLASS_KEYWORD) {
            PsiBuilder.Marker mark = builder.mark();
            getStatementParsing().parseClassWithMarker(builder, builder.mark(), false);
            mark.done(JSElementTypes.CLASS_EXPRESSION);
            return true;
        }

        else if (builder.getTokenType() == JSTokenTypes.BACKQUOTE) {
            parseStringTemplate(builder);
            return true;
        }
        else if (getStatementParsing().isAsyncFunction(builder)) {
            getStatementParsing().parseAsyncFunction(builder, builder.mark(), true);
            return true;
        }
        else if (isContextKeyword(builder, JSTokenTypes.ASYNC_KEYWORD) && canParseAsyncLambdaExpression(builder)) {
            // async x => ..., async (a, b) => ...
            PsiBuilder.Marker mark = builder.mark();
            advanceContextKeyword(builder, JSTokenTypes.ASYNC_KEYWORD);
            return parseLambdaExpression(builder, mark);
        }

        if (canParseLambdaExpression(builder)) {
            return parseLambdaExpression(builder);
        }
        return super.parsePrimaryExpression(builder);
    }

    @Override
    protected @Nullable IElementType parseMemberExpressionTail(PsiBuilder builder) {
        if (builder.getTokenType() == JSTokenTypes.BACKQUOTE) {
            parseStringTemplate(builder);
            return EcmaScript6ElementTypes.TAGGED_TEMPLATE_EXPRESSION;
        }
        return super.parseMemberExpressionTail(builder);
    }

    private void parseStringTemplate(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();
        builder.advanceLexer();

        while (true) {
            IElementType tokenType = builder.getTokenType();
            if (tokenType == JSTokenTypes.STRING_TEMPLATE_PART) {
                builder.advanceLexer();
            }
            else if (tokenType == JSTokenTypes.DOLLAR) {
                builder.advanceLexer();
                checkMatches(builder, JSTokenTypes.LBRACE, JavaScriptLocalize.javascriptParserMessageExpectedLbrace());
                if (!parseExpressionOptional(builder)) {
                    builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
                }
                if (builder.getTokenType() != JSTokenTypes.RBRACE) {
                    PsiBuilder.Marker error = builder.mark();
                    while (!builder.eof() && builder.getTokenType() != JSTokenTypes.RBRACE) {
                        builder.advanceLexer();
                    }
                    error.error(JavaScriptLocalize.javascriptParserMessageExpectedRbrace());
                }
                if (builder.getTokenType() == JSTokenTypes.RBRACE) {
                    builder.advanceLexer();
                }
            }
            else if (tokenType == JSTokenTypes.BACKQUOTE) {
                builder.advanceLexer();
                break;
            }
            else {
                builder.error(JavaScriptLocalize.javascriptParserMessageUnclosedStringLiteral());
                break;
            }
        }

        marker.done(EcmaScript6ElementTypes.STRING_TEMPLATE_EXPRESSION);
    }

    private boolean parseLambdaExpression(PsiBuilder builder) {
        return parseLambdaExpression(builder, builder.mark());
    }

    private boolean parseLambdaExpression(PsiBuilder builder, PsiBuilder.Marker mark) {
        IElementType tokenType = builder.getTokenType();
        if (tokenType == JSTokenTypes.IDENTIFIER) {
            PsiBuilder.Marker parameterList = builder.mark();
            getFunctionParsing().parseParameter(builder, null);
            parameterList.done(JSElementTypes.PARAMETER_LIST);
        }
        else {
            getFunctionParsing().parseParameterList(builder);
        }

        if (builder.getTokenType() == JSTokenTypes.DARROW) {
            builder.advanceLexer();
        }
        else {
            mark.rollbackTo();
            return false;
        }

        if (builder.getTokenType() == JSTokenTypes.LBRACE) {
            getStatementParsing().parseBlock(builder);
        }
        else {
            parseAssignmentExpression(builder);
        }

        mark.done(JSElementTypes.LAMBDA_EXPRESSION);
        return true;
    }

    private boolean canParseAsyncLambdaExpression(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();
        try {
            builder.advanceLexer();
            return canParseLambdaExpression(builder);
        }
        finally {
            marker.rollbackTo();
        }
    }

    /**
     * {@code await expr} is a unary expression (await foo.bar() awaits the call); await stays an identifier where an
     * operand can't follow (await = 1, f(await)).
     */
    @Override
    protected boolean parseUnaryExpression(PsiBuilder builder) {
        if (isContextKeyword(builder, JSTokenTypes.AWAIT_KEYWORD) && isOperandStart(builder.lookAhead(1))) {
            PsiBuilder.Marker expr = builder.mark();
            advanceContextKeyword(builder, JSTokenTypes.AWAIT_KEYWORD);
            if (!parseUnaryExpression(builder)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
            }
            expr.done(JSElementTypes.PREFIX_EXPRESSION);
            return true;
        }
        return super.parseUnaryExpression(builder);
    }

    private static boolean isOperandStart(@Nullable IElementType next) {
        return next != null
            && !JSTokenTypes.ASSIGNMENT_OPERATIONS.contains(next)
            && next != JSTokenTypes.SEMICOLON && next != JSTokenTypes.COMMA && next != JSTokenTypes.COLON
            && next != JSTokenTypes.RPAR && next != JSTokenTypes.RBRACKET && next != JSTokenTypes.RBRACE
            && next != JSTokenTypes.DOT && next != JSTokenTypes.DARROW && next != JSTokenTypes.QUEST;
    }

    private boolean canParseLambdaExpression(PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();
        try {
            IElementType tokenType = builder.getTokenType();
            if (tokenType == JSTokenTypes.IDENTIFIER) {
                builder.advanceLexer();
                if (builder.getTokenType() == JSTokenTypes.DARROW) {
                    return true;
                }
            }
            else if (tokenType == JSTokenTypes.LPAR) {
                getFunctionParsing().parseParameterList(builder);

                if (builder.getTokenType() == JSTokenTypes.DARROW) {
                    return true;
                }
            }
            return false;
        }
        finally {
            marker.rollbackTo();
        }
    }

    @Override
    protected void parseProperty(PsiBuilder builder) {
        IElementType nameTokenType = builder.getTokenType();
        PsiBuilder.Marker propertyMark = builder.mark();

        IElementType isSetterOrGetterType = null;

        if (nameTokenType == JSTokenTypes.LBRACKET) {
            PsiBuilder.Marker mark = builder.mark();
            builder.advanceLexer();
            parseExpression(builder);
            checkMatches(builder, JSTokenTypes.RBRACKET, JavaScriptLocalize.javascriptParserMessageExpectedRbracket());
            mark.done(JSElementTypes.COMPUTED_NAME);
        }
        else {
            if (isNotPropertyStart(builder, nameTokenType)) {
                builder.error(JavaScriptLocalize.javascriptParserMessageExpectedIdentifierStringLiteralOrNumericLiteral());
            }

            // async m() {} / async *m() {}
            if (isContextKeyword(builder, JSTokenTypes.ASYNC_KEYWORD)
                && (builder.lookAhead(1) == JSTokenTypes.MULT || builder.lookAhead(1) == JSTokenTypes.IDENTIFIER)
                && (builder.lookAhead(2) == JSTokenTypes.LPAR || builder.lookAhead(1) == JSTokenTypes.MULT)) {
                advanceContextKeyword(builder, JSTokenTypes.ASYNC_KEYWORD);
                if (builder.getTokenType() == JSTokenTypes.MULT) {
                    builder.advanceLexer();
                }
                nameTokenType = builder.getTokenType();
            }

            IElementType setOrGetToken = isContextKeyword(builder, JSTokenTypes.GET_SET_TOKEN_SET);

            if (setOrGetToken != null && builder.lookAhead(1) == JSTokenTypes.IDENTIFIER) {
                isSetterOrGetterType = setOrGetToken;

                advanceContextKeyword(builder, isSetterOrGetterType);
            }
            else {
                // just advance identifier
                builder.advanceLexer();
            }
        }

        IElementType doneElement = JSElementTypes.PROPERTY;

        if (isSetterOrGetterType != null) {
            // advance name - see isSetterOrGetterType set
            builder.advanceLexer();

            getFunctionParsing().parseParameterList(builder);

            getStatementParsing().parseFunctionBody(builder);

            doneElement = JSElementTypes.FUNCTION_PROPERTY;
        }
        else {
            IElementType nextTokenType = builder.getTokenType();
            // we finished property
            if ((nextTokenType == JSTokenTypes.COMMA || nextTokenType == JSTokenTypes.RBRACE) && nameTokenType == JSTokenTypes.IDENTIFIER) {
                propertyMark.rollbackTo(); // rollback it

                propertyMark = builder.mark();

                PsiBuilder.Marker referenceMark = builder.mark();
                builder.advanceLexer();
                referenceMark.done(JSElementTypes.REFERENCE_EXPRESSION);
            }
            else if (nextTokenType == JSTokenTypes.LPAR) {
                getFunctionParsing().parseParameterList(builder);

                getStatementParsing().parseFunctionBody(builder);

                doneElement = JSElementTypes.FUNCTION_PROPERTY;
            }
            else {
                checkMatches(builder, JSTokenTypes.COLON, JavaScriptLocalize.javascriptParserMessageExpectedColon());

                builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, Boolean.TRUE);
                if (!parseAssignmentExpression(builder)) {
                    builder.error(JavaScriptLocalize.javascriptParserMessageExpectedExpression());
                }
                builder.putUserData(WITHIN_OBJECT_LITERAL_EXPRESSION, null);
            }
        }

        propertyMark.done(doneElement);
    }

    @Override
    public boolean isNotPropertyStart(PsiBuilder builder, IElementType elementType) {
        return super.isNotPropertyStart(builder, elementType) && elementType != JSTokenTypes.LBRACKET;
    }
}
