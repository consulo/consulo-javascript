package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.ui.SingleIntegerFieldOptionsPanel;
import javax.annotation.Nonnull;

import javax.swing.*;

public class OverlyComplexBooleanExpressionJSInspection extends JavaScriptInspection {
    private static final int TERM_LIMIT = 3;

    /**
     * @noinspection PublicField
     */
    public int m_limit = TERM_LIMIT;

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("overly.complex.boolean.expression.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    private int getLimit() {
        return m_limit;
    }

    @Override
	public JComponent createOptionsPanel() {
        return new SingleIntegerFieldOptionsPanel(InspectionJSBundle.message("maximum.number.of.terms.parameter"),
                this, "m_limit");
    }

    @Override
	protected boolean buildQuickFixesOnlyForOnTheFlyErrors() {
        return true;
    }

    @Override
	protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("overly.complex.boolean.expression.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            checkExpression(expression);
        }

        @Override public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            checkExpression(expression);
        }

        @Override public void visitJSParenthesizedExpression(JSParenthesizedExpression expression) {
            super.visitJSParenthesizedExpression(expression);
            checkExpression(expression);
        }

        private void checkExpression(JSExpression expression) {
            if (!isBoolean(expression)) {
                return;
            }
            if (isParentBoolean(expression)) {
                return;
            }
            final int numTerms = countTerms(expression);
            if (numTerms <= getLimit()) {
                return;
            }
            registerError(expression);
        }

        private int countTerms(JSExpression expression) {
            if (expression == null) {
                return 0;
            }
            if (!isBoolean(expression)) {
                return 1;
            }
            if (expression instanceof JSBinaryExpression) {
                final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
                final JSExpression lhs = binaryExpression.getLOperand();
                final JSExpression rhs = binaryExpression.getROperand();
                return countTerms(lhs) + countTerms(rhs);
            } else if (expression instanceof JSPrefixExpression) {
                final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
                final JSExpression operand = prefixExpression.getExpression();
                return countTerms(operand);
            } else if (expression instanceof JSParenthesizedExpression) {
                final JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) expression;
                final JSExpression contents = parenthesizedExpression.getInnerExpression();
                return countTerms(contents);
            }
            return 1;
        }

        private boolean isParentBoolean(JSExpression expression) {
            final PsiElement parent = expression.getParent();
            if (!(parent instanceof JSExpression)) {
                return false;
            }
            return isBoolean((JSExpression) parent);
        }

        private boolean isBoolean(JSExpression expression) {
            if (expression instanceof JSBinaryExpression) {
                final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
                final IElementType sign = binaryExpression.getOperationSign();
                return JSTokenTypes.ANDAND.equals(sign) ||
                        JSTokenTypes.OROR.equals(sign);
            } else if (expression instanceof JSPrefixExpression) {
                final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
                final IElementType sign = prefixExpression.getOperationSign();
                return JSTokenTypes.EXCL.equals(sign);
            } else if (expression instanceof JSParenthesizedExpression) {
                final JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) expression;
                final JSExpression contents = parenthesizedExpression.getInnerExpression();
                return isBoolean(contents);
            }
            return false;
        }
    }
}
