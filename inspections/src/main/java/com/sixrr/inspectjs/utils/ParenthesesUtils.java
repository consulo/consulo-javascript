package com.sixrr.inspectjs.utils;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.ast.IElementType;

import java.util.HashMap;
import java.util.Map;

public class ParenthesesUtils {
    private ParenthesesUtils() {
    }

    private static final int PARENTHESIZED_PRECEDENCE = 0;
    private static final int LITERAL_PRECEDENCE = 0;
    public static final int METHOD_CALL_PRECEDENCE = 1;

    private static final int POSTFIX_PRECEDENCE = 2;
    public static final int PREFIX_PRECEDENCE = 3;
    public static final int TYPE_CAST_PRECEDENCE = 4;
    public static final int MULTIPLICATIVE_PRECEDENCE = 5;
    private static final int ADDITIVE_PRECEDENCE = 6;
    public static final int SHIFT_PRECEDENCE = 7;
    private static final int RELATIONAL_PRECEDENCE = 8;
    private static final int EQUALITY_PRECEDENCE = 9;

    private static final int BINARY_AND_PRECEDENCE = 10;
    private static final int BINARY_XOR_PRECEDENCE = 11;
    private static final int BINARY_OR_PRECEDENCE = 12;
    public static final int AND_PRECEDENCE = 13;
    public static final int OR_PRECEDENCE = 14;
    public static final int CONDITIONAL_PRECEDENCE = 15;
    private static final int ASSIGNMENT_PRECEDENCE = 16;

    private static final int NUM_PRECEDENCES = 17;

    private static final Map<IElementType, Integer> s_binaryOperatorPrecedence =
            new HashMap<IElementType, Integer>(NUM_PRECEDENCES);

    static {
        s_binaryOperatorPrecedence.put(JSTokenTypes.PLUS, ADDITIVE_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.MINUS, ADDITIVE_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.MULT, MULTIPLICATIVE_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.DIV, MULTIPLICATIVE_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.PERC, MULTIPLICATIVE_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.ANDAND, AND_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.OROR, OR_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.AND, BINARY_AND_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.OR, BINARY_OR_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.XOR, BINARY_XOR_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.LTLT, SHIFT_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.GTGT, SHIFT_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.GTGTGT, SHIFT_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.GT, RELATIONAL_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.GE, RELATIONAL_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.LT, RELATIONAL_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.LE, RELATIONAL_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.EQEQ, EQUALITY_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.EQEQEQ, EQUALITY_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.NE, EQUALITY_PRECEDENCE);
        s_binaryOperatorPrecedence.put(JSTokenTypes.NEQEQ, EQUALITY_PRECEDENCE);
    }

    public static int getPrecendence(JSExpression exp) {
        if (exp instanceof JSThisExpression ||
                exp instanceof JSLiteralExpression ||
                exp instanceof JSObjectLiteralExpression ||
                exp instanceof JSArrayLiteralExpression ||
                exp instanceof JSIndexedPropertyAccessExpression) {
            return LITERAL_PRECEDENCE;
        }
        if (exp instanceof JSReferenceExpression) {
            if (((JSReferenceExpression) exp).getQualifier() != null) {
                return METHOD_CALL_PRECEDENCE;
            } else {
                return LITERAL_PRECEDENCE;
            }
        }
        if (exp instanceof JSCallExpression) {
            return METHOD_CALL_PRECEDENCE;
        }
        if (exp instanceof JSPrefixExpression) {
            return PREFIX_PRECEDENCE;
        }
        if (exp instanceof JSPostfixExpression) {
            return POSTFIX_PRECEDENCE;
        }
        if (exp instanceof JSBinaryExpression && !(exp instanceof JSAssignmentExpression)) {
            final IElementType sign = ((JSBinaryExpression) exp)
                    .getOperationSign();
            return precedenceForBinaryOperator(sign);
        }
        if (exp instanceof JSConditionalExpression) {
            return CONDITIONAL_PRECEDENCE;
        }
        if (exp instanceof JSAssignmentExpression) {
            return ASSIGNMENT_PRECEDENCE;
        }
        if (exp instanceof JSParenthesizedExpression) {
            return PARENTHESIZED_PRECEDENCE;
        }
        return -1;
    }

    private static int precedenceForBinaryOperator(IElementType sign) {
        if (s_binaryOperatorPrecedence.containsKey(sign)) return s_binaryOperatorPrecedence.get(sign);
        return ASSIGNMENT_PRECEDENCE;
    }

    public static JSExpression stripExpression(JSExpression expression) {
        JSExpression workingExpression = expression;
        while (workingExpression instanceof JSParenthesizedExpression) {
            final JSParenthesizedExpression parenthesizedExpression =
                    (JSParenthesizedExpression) workingExpression;
            workingExpression = parenthesizedExpression.getInnerExpression();
        }
        return workingExpression;
    }

    public static JSExpression stripParentheses(JSExpression exp) {
        JSExpression parenthesized = exp;
        while (parenthesized instanceof JSParenthesizedExpression) {
            parenthesized = ((JSParenthesizedExpression) parenthesized)
                    .getInnerExpression();
        }

        return parenthesized;
    }}
