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

import javax.annotation.Nullable;

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import consulo.language.ast.IElementType;

public class BoolUtils {

    @NonNls public static final String TRUE  = "true";
    @NonNls public static final String FALSE = "false";

    private BoolUtils() {}

    public static boolean isNegated(JSExpression condition) {
        return (findNegation(condition) != null);
    }

    @Nullable
    public static JSExpression findNegation(JSExpression condition) {
        JSExpression ancestor = condition;

        while (ancestor.getParent() instanceof JSParenthesizedExpression) {
            ancestor = (JSExpression) ancestor.getParent();
        }

        if (ancestor.getParent() instanceof JSPrefixExpression) {
            final JSPrefixExpression prefixAncestor = (JSPrefixExpression) ancestor.getParent();
            if (prefixAncestor.getOperationSign().equals(JSTokenTypes.EXCL)) {
                return prefixAncestor;
            }
        }

        return null;
    }

    public static boolean isNegation(JSExpression condition) {
        return isNegation(condition, false);
    }

    public static boolean isNegation(JSExpression condition, boolean ignoreNegatedNullComparison) {
            if (condition instanceof JSPrefixExpression) {
                final JSPrefixExpression prefixExpression = (JSPrefixExpression) condition;
                final IElementType sign             = prefixExpression.getOperationSign();

                return sign.equals(JSTokenTypes.EXCL);
            } else if (condition instanceof JSBinaryExpression) {
                final JSBinaryExpression binaryExpression = (JSBinaryExpression) condition;
                final IElementType sign = binaryExpression.getOperationSign();
                final JSExpression lhs  = binaryExpression.getLOperand();
                final JSExpression rhs  = binaryExpression.getROperand();

                if (rhs != null && sign.equals(JSTokenTypes.NE)) {
                    if (!ignoreNegatedNullComparison) {
                        return true;
                    }

                    return !isNullLiteral(lhs) && !isNullLiteral(rhs);
                } else {
                    return false;
                }
            } else if (condition instanceof JSParenthesizedExpression) {
                return isNegation(((JSParenthesizedExpression) condition).getInnerExpression());
            } else {
                return false;
            }
    }

  private static boolean isNullLiteral(final JSExpression lhs) {
    return lhs.getNode() != null && JSTokenTypes.NULL_KEYWORD.equals(lhs.getNode().getElementType());
  }

  public static JSExpression getNegated(JSExpression condition) {
        if (condition instanceof JSPrefixExpression) {
            final JSPrefixExpression prefixExp = (JSPrefixExpression) condition;
            final JSExpression       operand   = prefixExp.getExpression();

            return ParenthesesUtils.stripParentheses(operand);
        } else if (condition instanceof JSBinaryExpression) {
            final JSBinaryExpression binaryExpression = (JSBinaryExpression) condition;
            final IElementType sign        = binaryExpression.getOperationSign();
            final JSExpression lhs         = binaryExpression.getLOperand();
            final JSExpression rhs         = binaryExpression.getROperand();
            final String       negatedSign = ComparisonUtils.getNegatedOperatorText(sign);
            final String       negatedText = lhs.getText() + negatedSign + rhs.getText();

            return (JSExpression) JSChangeUtil.createExpressionFromText(
              condition.getProject(),
              negatedText);
        } else if (condition instanceof JSParenthesizedExpression) {
            return getNegated(((JSParenthesizedExpression) condition).getInnerExpression());
        }
        return condition;
    }

    public static boolean isBooleanLiteral(JSExpression condition) {
        if (!(condition instanceof JSLiteralExpression)) {
            return false;
        }

        final String  text = condition.getText();

        return (TRUE.equals(text) || FALSE.equals(text));
    }

    public static Boolean getBooleanLiteral(JSExpression condition) {
        if (!(condition instanceof JSLiteralExpression)) {
            return null;
        }

        final String text = condition.getText();

        return (TRUE .equals(text) ? Boolean.TRUE :
                FALSE.equals(text) ? Boolean.FALSE
                                   : null);
    }

    public static boolean isTrue(@Nullable JSExpression condition) {
        return (condition instanceof JSLiteralExpression &&
                TRUE.equals(condition.getText()));
    }

    public static boolean isFalse(@Nullable JSExpression condition) {
        return (condition instanceof JSLiteralExpression &&
                FALSE.equals(condition.getText()));
    }

    public static String getNegatedExpressionText(JSExpression condition) {
        if (BoolUtils.isNegation(condition)) {
            return ParenthesesUtils.getParenthesized(BoolUtils.getNegated(condition), ParenthesesUtils.OR_PRECENDENCE);
        } else if(ComparisonUtils.isComparisonOperator(condition)) {
            final JSBinaryExpression binaryExpression  = (JSBinaryExpression) condition;
            final IElementType       sign              = binaryExpression.getOperationSign();
            final String             negatedComparison = ComparisonUtils.getNegatedOperatorText(sign);
            final JSExpression       leftOperand       = binaryExpression.getLOperand();
            final JSExpression       rightOperand      = binaryExpression.getROperand();

            return leftOperand.getText() + negatedComparison + (rightOperand != null ? rightOperand.getText():"");
        } else {
            return '!' + ParenthesesUtils.getParenthesized(condition, ParenthesesUtils.PREFIX_PRECENDENCE);
        }
    }
}
