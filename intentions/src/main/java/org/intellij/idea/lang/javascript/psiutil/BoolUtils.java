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
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.language.ast.IElementType;
import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

public class BoolUtils {
    @NonNls
    public static final String TRUE = "true";
    @NonNls
    public static final String FALSE = "false";

    private BoolUtils() {
    }

    public static boolean isNegated(JSExpression condition) {
        return (findNegation(condition) != null);
    }

    @Nullable
    public static JSExpression findNegation(JSExpression condition) {
        JSExpression ancestor = condition;

        while (ancestor.getParent() instanceof JSParenthesizedExpression) {
            ancestor = (JSExpression)ancestor.getParent();
        }

        if (ancestor.getParent() instanceof JSPrefixExpression) {
            JSPrefixExpression prefixAncestor = (JSPrefixExpression)ancestor.getParent();
            if (JSTokenTypes.EXCL.equals(prefixAncestor.getOperationSign())) {
                return prefixAncestor;
            }
        }

        return null;
    }

    public static boolean isNegation(JSExpression condition) {
        return isNegation(condition, false);
    }

    public static boolean isNegation(JSExpression condition, boolean ignoreNegatedNullComparison) {
        if (condition instanceof JSPrefixExpression prefixExpression) {
            IElementType sign = prefixExpression.getOperationSign();

            return JSTokenTypes.EXCL.equals(sign);
        }
        else if (condition instanceof JSBinaryExpression binaryExpression) {
            IElementType sign = binaryExpression.getOperationSign();
            JSExpression lhs = binaryExpression.getLOperand();
            JSExpression rhs = binaryExpression.getROperand();

            if (rhs != null && JSTokenTypes.NE.equals(sign)) {
                if (!ignoreNegatedNullComparison) {
                    return true;
                }

                return !isNullLiteral(lhs) && !isNullLiteral(rhs);
            }
            else {
                return false;
            }
        }
        else if (condition instanceof JSParenthesizedExpression parenExp) {
            return isNegation(parenExp.getInnerExpression());
        }
        else {
            return false;
        }
    }

    private static boolean isNullLiteral(JSExpression lhs) {
        return lhs.getNode() != null && JSTokenTypes.NULL_KEYWORD.equals(lhs.getNode().getElementType());
    }

    public static JSExpression getNegated(JSExpression condition) {
        if (condition instanceof JSPrefixExpression prefixExp) {
            JSExpression operand = prefixExp.getExpression();

            return ParenthesesUtils.stripParentheses(operand);
        }
        else if (condition instanceof JSBinaryExpression binaryExpression) {
            IElementType sign = binaryExpression.getOperationSign();
            JSExpression lhs = binaryExpression.getLOperand();
            JSExpression rhs = binaryExpression.getROperand();
            String negatedSign = ComparisonUtils.getNegatedOperatorText(sign);
            String negatedText = lhs.getText() + negatedSign + rhs.getText();

            return JSChangeUtil.createExpressionFromText(condition.getProject(), negatedText);
        }
        else if (condition instanceof JSParenthesizedExpression parenExp) {
            return getNegated(parenExp.getInnerExpression());
        }
        return condition;
    }

    public static boolean isBooleanLiteral(JSExpression condition) {
        if (!(condition instanceof JSLiteralExpression)) {
            return false;
        }

        String text = condition.getText();

        return (TRUE.equals(text) || FALSE.equals(text));
    }

    public static Boolean getBooleanLiteral(JSExpression condition) {
        if (!(condition instanceof JSLiteralExpression)) {
            return null;
        }

        String text = condition.getText();

        return TRUE.equals(text) ? Boolean.TRUE
            : FALSE.equals(text) ? Boolean.FALSE
            : null;
    }

    public static boolean isTrue(@Nullable JSExpression condition) {
        return condition instanceof JSLiteralExpression && TRUE.equals(condition.getText());
    }

    public static boolean isFalse(@Nullable JSExpression condition) {
        return condition instanceof JSLiteralExpression && FALSE.equals(condition.getText());
    }

    public static String getNegatedExpressionText(JSExpression condition) {
        if (BoolUtils.isNegation(condition)) {
            return ParenthesesUtils.getParenthesized(BoolUtils.getNegated(condition), ParenthesesUtils.OR_PRECENDENCE);
        }
        else if (ComparisonUtils.isComparisonOperator(condition)) {
            JSBinaryExpression binaryExpression = (JSBinaryExpression)condition;
            IElementType sign = binaryExpression.getOperationSign();
            String negatedComparison = ComparisonUtils.getNegatedOperatorText(sign);
            JSExpression leftOperand = binaryExpression.getLOperand();
            JSExpression rightOperand = binaryExpression.getROperand();

            return leftOperand.getText() + negatedComparison + (rightOperand != null ? rightOperand.getText() : "");
        }
        else {
            return '!' + ParenthesesUtils.getParenthesized(condition, ParenthesesUtils.PREFIX_PRECENDENCE);
        }
    }
}
