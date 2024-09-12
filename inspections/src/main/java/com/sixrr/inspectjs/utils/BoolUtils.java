package com.sixrr.inspectjs.utils;

import jakarta.annotation.Nonnull;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import consulo.language.ast.IElementType;

import jakarta.annotation.Nullable;
import org.jetbrains.annotations.NonNls;

public class BoolUtils {
    private BoolUtils() {
        super();
    }

    public static boolean isNegation(@Nonnull JSExpression exp) {
        if (!(exp instanceof JSPrefixExpression)) {
            return false;
        }
        final JSPrefixExpression prefixExp = (JSPrefixExpression)exp;
        final IElementType sign = prefixExp.getOperationSign();
        return JSTokenTypes.EXCL.equals(sign);
    }


    public static boolean isTrue(@Nullable JSExpression test) {
        if (test == null) {
            return false;
        }
        @NonNls final String text = test.getText();
        return "true".equals(text);
    }

    public static boolean isFalse(@Nullable JSExpression test) {
        if (test == null) {
            return false;
        }
        @NonNls final String text = test.getText();
        return "false".equals(text);
    }

    public static String getNegatedExpressionText(@Nonnull JSExpression condition) {
        if (condition instanceof JSParenthesizedExpression) {
            final JSExpression contentExpression = ((JSParenthesizedExpression)condition).getInnerExpression();
            return '(' + getNegatedExpressionText(contentExpression) + ')';
        }
        else if (BoolUtils.isNegation(condition)) {
            final JSExpression negated = getNegated(condition);
            return negated.getText();
        }
        else if (ComparisonUtils.isComparison(condition)) {
            final JSBinaryExpression binaryExpression = (JSBinaryExpression)condition;
            final IElementType sign = binaryExpression.getOperationSign();
            final String negatedComparison = ComparisonUtils.getNegatedComparison(sign);
            final JSExpression lhs = binaryExpression.getLOperand();
            final JSExpression rhs = binaryExpression.getROperand();
            assert rhs != null;
            return lhs.getText() + negatedComparison + rhs.getText();
        }
        else if (ParenthesesUtils.getPrecendence(condition) >
            ParenthesesUtils.PREFIX_PRECEDENCE) {
            return "!(" + condition.getText() + ')';
        }
        else {
            return '!' + condition.getText();
        }
    }

    private static JSExpression getNegated(@Nonnull JSExpression exp) {
        final JSPrefixExpression prefixExp = (JSPrefixExpression)exp;
        final JSExpression operand = prefixExp.getExpression();
        return ParenthesesUtils.stripParentheses(operand);
    }
}
