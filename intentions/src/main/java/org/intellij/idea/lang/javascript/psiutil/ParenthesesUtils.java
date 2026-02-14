/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.psiutil;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;

import java.util.HashMap;
import java.util.Map;

public class ParenthesesUtils {
    private ParenthesesUtils() {
    }

    private static final int PARENTHESIZED_PRECENDENCE = 0;
    private static final int LITERAL_PRECENDENCE = 0;
    public static final int METHOD_CALL_PRECENDENCE = 1;

    private static final int POSTFIX_PRECENDENCE = 2;
    public static final int PREFIX_PRECENDENCE = 3;
    public static final int TYPE_CAST_PRECENDENCE = 4;
    public static final int MULTIPLICATIVE_PRECENDENCE = 5;
    private static final int ADDITIVE_PRECENDENCE = 6;
    public static final int SHIFT_PRECENDENCE = 7;
    private static final int RELATIONAL_PRECENDENCE = 8;
    public static final int EQUALITY_PRECENDENCE = 9;

    private static final int BINARY_AND_PRECENDENCE = 10;
    private static final int BINARY_XOR_PRECENDENCE = 11;
    private static final int BINARY_OR_PRECENDENCE = 12;
    public static final int AND_PRECENDENCE = 13;
    public static final int OR_PRECENDENCE = 14;
    public static final int CONDITIONAL_PRECENDENCE = 15;
    private static final int ASSIGNMENT_PRECENDENCE = 16;

    private static final int NUM_PRECENDENCES = 17;

    private static final Map<IElementType, Integer> binaryOperatorPrecendence =
        new HashMap<IElementType, Integer>(NUM_PRECENDENCES);

    static {
        binaryOperatorPrecendence.put(JSTokenTypes.PLUS, ADDITIVE_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.MINUS, ADDITIVE_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.MULT, MULTIPLICATIVE_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.DIV, MULTIPLICATIVE_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.PERC, MULTIPLICATIVE_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.ANDAND, AND_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.OROR, OR_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.AND, BINARY_AND_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.OR, BINARY_OR_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.XOR, BINARY_XOR_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.COMMA, ASSIGNMENT_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.LTLT, SHIFT_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.GTGT, SHIFT_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.GTGTGT, SHIFT_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.GT, RELATIONAL_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.GE, RELATIONAL_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.LT, RELATIONAL_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.LE, RELATIONAL_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.EQEQ, EQUALITY_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.NE, EQUALITY_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.EQEQEQ, EQUALITY_PRECENDENCE);
        binaryOperatorPrecendence.put(JSTokenTypes.NEQEQ, EQUALITY_PRECENDENCE);
    }

    public static JSExpression stripParentheses(JSExpression expression) {
        JSExpression parenthesized = expression;

        if (parenthesized != null) {
            while (parenthesized instanceof JSParenthesizedExpression) {
                parenthesized = ((JSParenthesizedExpression)parenthesized).getInnerExpression();
            }
        }

        return parenthesized;
    }

    public static JSExpression unstripParentheses(JSExpression expression) {
        JSExpression parenthesized = expression;

        if (parenthesized != null) {
            PsiElement parent = parenthesized.getParent();

            while (parent instanceof JSParenthesizedExpression parenExp) {
                parenthesized = parenExp;
                parent = parenthesized.getParent();
            }
        }

        return parenthesized;
    }

    public static String getParenthesized(JSExpression expression, int precendence) {
        return ParenthesesUtils.getPrecendence(expression) > precendence
            ? '(' + expression.getText() + ')'
            : expression.getText();
    }

    public static int getPrecendence(JSExpression expression) {
        if (expression instanceof JSThisExpression
            || expression instanceof JSLiteralExpression
            || expression instanceof JSArrayLiteralExpression
            || expression instanceof JSObjectLiteralExpression
            || expression instanceof JSIndexedPropertyAccessExpression) {
            return LITERAL_PRECENDENCE;
        }
        if (expression instanceof JSReferenceExpression referenceExpression) {
            return referenceExpression.getQualifier() == null
                ? LITERAL_PRECENDENCE
                : METHOD_CALL_PRECENDENCE;
        }
        if (expression instanceof JSNewExpression) {
            return TYPE_CAST_PRECENDENCE;
        }
        if (expression instanceof JSCallExpression) {
            return METHOD_CALL_PRECENDENCE;
        }
        if (expression instanceof JSPostfixExpression) {
            return POSTFIX_PRECENDENCE;
        }
        if (expression instanceof JSPrefixExpression) {
            return PREFIX_PRECENDENCE;
        }
        if (expression instanceof JSAssignmentExpression) {
            return ASSIGNMENT_PRECENDENCE;
        }
        if (expression instanceof JSBinaryExpression binaryExpression) {
            return getBinaryOperatorPrecendence(binaryExpression.getOperationSign());
        }
        if (expression instanceof JSConditionalExpression) {
            return CONDITIONAL_PRECENDENCE;
        }
        if (expression instanceof JSParenthesizedExpression) {
            return PARENTHESIZED_PRECENDENCE;
        }

        // expression instanceof JSCommaExpression      ||
        // expression instanceof JSDefinitionExpression ||
        // expression instanceof JSFunctionExpression
        return -1;
    }

    private static int getBinaryOperatorPrecendence(IElementType sign) {
        if (binaryOperatorPrecendence.containsKey(sign)) {
            return binaryOperatorPrecendence.get(sign);
        }
        return ASSIGNMENT_PRECENDENCE;
    }

    public static String removeParentheses(JSExpression expression) {
        if (expression instanceof JSCallExpression callExpression) {
            return removeParensFromFunctionCallExpression(callExpression);
        }
        if (expression instanceof JSReferenceExpression referenceExpression) {
            return removeParensFromReferenceExpression(referenceExpression);
        }
        if (expression instanceof JSAssignmentExpression assignmentExpression) {
            return removeParensFromAssignmentExpression(assignmentExpression);
        }
        if (expression instanceof JSArrayLiteralExpression arrayLiteralExpression) {
            return removeParensFromArrayLiteralExpression(arrayLiteralExpression);
        }
        if (expression instanceof JSPrefixExpression prefixExpression) {
            return removeParensFromPrefixExpression(prefixExpression);
        }
        if (expression instanceof JSPostfixExpression postfixExpression) {
            return removeParensFromPostfixExpression(postfixExpression);
        }
        if (expression instanceof JSBinaryExpression binaryExpression) {
            return removeParensFromBinaryExpression(binaryExpression);
        }
        if (expression instanceof JSConditionalExpression conditionalExpression) {
            return removeParensFromConditionalExpression(conditionalExpression);
        }
        if (expression instanceof JSParenthesizedExpression parenthesizedExpression) {
            return removeParensFromParenthesizedExpression(parenthesizedExpression);
        }

        return expression.getText();
    }

    private static String removeParensFromReferenceExpression(JSReferenceExpression expression) {
        JSExpression qualifier = expression.getQualifier();

        if (qualifier != null) {
            return removeParentheses(qualifier) + '.' + expression.getReferencedName();
        }
        else {
            return expression.getText();
        }
    }

    private static String removeParensFromParenthesizedExpression(JSParenthesizedExpression parenthesizedExp) {
        JSExpression body = stripParentheses(parenthesizedExp.getInnerExpression());

        if (!(parenthesizedExp.getParent() instanceof JSExpression)) {
            return removeParentheses(body);
        }

        JSExpression parentExp = (JSExpression)parenthesizedExp.getParent();
        int parentPrecendence = getPrecendence(parentExp);
        int childPrecendence = getPrecendence(body);

        if (parentPrecendence < childPrecendence) {
            return '(' + removeParentheses(body) + ')';
        }
        else if (parentPrecendence == childPrecendence) {
            if (parentExp instanceof JSBinaryExpression parentBiExp && body instanceof JSBinaryExpression bodyBiExp) {
                IElementType parentOperator = parentBiExp.getOperationSign();
                IElementType bodyOperator = bodyBiExp.getOperationSign();
                JSExpression lhs = parentBiExp.getLOperand();

                if (lhs.equals(parenthesizedExp) && parentOperator.equals(bodyOperator)) {
                    return removeParentheses(body);
                }
                else {
                    return '(' + removeParentheses(body) + ')';
                }
            }
            else {
                return removeParentheses(body);
            }
        }
        else {
            return removeParentheses(body);
        }
    }

    private static String removeParensFromConditionalExpression(JSConditionalExpression conditionalExp) {
        JSExpression condition = conditionalExp.getCondition();
        JSExpression thenBranch = conditionalExp.getThen();
        JSExpression elseBranch = conditionalExp.getElse();

        return removeParentheses(condition) + " ? " +
            removeParentheses(thenBranch) + " : " +
            removeParentheses(elseBranch);
    }

    private static String removeParensFromBinaryExpression(JSBinaryExpression binaryExp) {
        JSExpression lhs = binaryExp.getLOperand();
        JSExpression rhs = binaryExp.getROperand();
        IElementType sign = binaryExp.getOperationSign();

        return removeParentheses(lhs) + ' ' + BinaryOperatorUtils.getOperatorText(sign) + ' ' + removeParentheses(rhs);
    }

    private static String removeParensFromPostfixExpression(JSPostfixExpression postfixExp) {
        JSExpression body = postfixExp.getExpression();
        IElementType sign = postfixExp.getOperationSign();

        return removeParentheses(body) + BinaryOperatorUtils.getOperatorText(sign);
    }

    private static String removeParensFromPrefixExpression(JSPrefixExpression prefixExp) {
        JSExpression body = prefixExp.getExpression();
        IElementType sign = prefixExp.getOperationSign();
        if (sign == null) {
            ASTNode[] astNodes = prefixExp.getNode().getChildren(JSTokenTypes.UNARY_OPERATIONS); // hack for 8.1
            sign = astNodes.length == 1 ? astNodes[0].getElementType() : null;
        }

        return (sign != null ? BinaryOperatorUtils.getOperatorText(sign) : "delete ") + removeParentheses(body);
    }

    private static String removeParensFromArrayLiteralExpression(JSArrayLiteralExpression init) {
        JSExpression[] contents = init.getExpressions();
        String text = init.getText();
        int textLength = text.length();
        StringBuilder out = new StringBuilder(textLength);

        out.append('(');
        for (int index = 0; index < contents.length; index++) {
            JSExpression arg = contents[index];

            if (index != 0) {
                out.append(',');
            }
            out.append(removeParentheses(arg));
        }

        return out.append(')').toString();
    }

    private static String removeParensFromAssignmentExpression(JSAssignmentExpression assignment) {
        JSExpression lhs = assignment.getLOperand();
        JSExpression rhs = assignment.getROperand();
        IElementType sign = assignment.getOperationSign();
        return removeParentheses(lhs) + ' ' +
            BinaryOperatorUtils.getOperatorText(sign) + ' ' +
            removeParentheses(rhs);
    }

    private static String removeParensFromFunctionCallExpression(JSCallExpression functionCall) {
        JSExpression target = functionCall.getMethodExpression();
        JSArgumentList argumentList = functionCall.getArgumentList();

        assert (argumentList != null);

        JSExpression[] args = argumentList.getArguments();
        String methodCallText = functionCall.getText();
        int length = methodCallText.length();
        StringBuilder out = new StringBuilder(length);
        String strippedTarget = removeParentheses(target);

        out.append(strippedTarget);
        out.append('(');
        for (int index = 0; index < args.length; index++) {
            JSExpression arg = args[index];
            if (index != 0) {
                out.append(',');
            }
            out.append(removeParentheses(arg));
        }
        out.append(')');
        return out.toString();
    }
}
