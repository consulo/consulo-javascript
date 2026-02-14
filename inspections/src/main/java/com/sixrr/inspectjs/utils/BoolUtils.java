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
        JSPrefixExpression prefixExp = (JSPrefixExpression)exp;
        IElementType sign = prefixExp.getOperationSign();
        return JSTokenTypes.EXCL.equals(sign);
    }


    public static boolean isTrue(@Nullable JSExpression test) {
        if (test == null) {
            return false;
        }
        @NonNls String text = test.getText();
        return "true".equals(text);
    }

    public static boolean isFalse(@Nullable JSExpression test) {
        if (test == null) {
            return false;
        }
        @NonNls String text = test.getText();
        return "false".equals(text);
    }

    public static String getNegatedExpressionText(@Nonnull JSExpression condition) {
        if (condition instanceof JSParenthesizedExpression) {
            JSExpression contentExpression = ((JSParenthesizedExpression)condition).getInnerExpression();
            return '(' + getNegatedExpressionText(contentExpression) + ')';
        }
        else if (BoolUtils.isNegation(condition)) {
            JSExpression negated = getNegated(condition);
            return negated.getText();
        }
        else if (ComparisonUtils.isComparison(condition)) {
            JSBinaryExpression binaryExpression = (JSBinaryExpression)condition;
            IElementType sign = binaryExpression.getOperationSign();
            String negatedComparison = ComparisonUtils.getNegatedComparison(sign);
            JSExpression lhs = binaryExpression.getLOperand();
            JSExpression rhs = binaryExpression.getROperand();
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
        JSPrefixExpression prefixExp = (JSPrefixExpression)exp;
        JSExpression operand = prefixExp.getExpression();
        return ParenthesesUtils.stripParentheses(operand);
    }
}
