package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class OverlyComplexBooleanExpressionJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.overlyComplexBooleanExpressionDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new OverlyComplexBooleanExpressionJSInspectionState();
    }

    @Override
    protected boolean buildQuickFixesOnlyForOnTheFlyErrors() {
        return true;
    }

    @RequiredReadAction
    @Override
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.overlyComplexBooleanExpressionErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<OverlyComplexBooleanExpressionJSInspectionState> {

        @Override
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            checkExpression(expression);
        }

        @Override
        public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            checkExpression(expression);
        }

        @Override
        public void visitJSParenthesizedExpression(JSParenthesizedExpression expression) {
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
            if (numTerms <= myState.m_limit) {
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
            }
            else if (expression instanceof JSPrefixExpression) {
                final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
                final JSExpression operand = prefixExpression.getExpression();
                return countTerms(operand);
            }
            else if (expression instanceof JSParenthesizedExpression) {
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
            }
            else if (expression instanceof JSPrefixExpression) {
                final JSPrefixExpression prefixExpression = (JSPrefixExpression) expression;
                final IElementType sign = prefixExpression.getOperationSign();
                return JSTokenTypes.EXCL.equals(sign);
            }
            else if (expression instanceof JSParenthesizedExpression) {
                final JSParenthesizedExpression parenthesizedExpression = (JSParenthesizedExpression) expression;
                final JSExpression contents = parenthesizedExpression.getInnerExpression();
                return isBoolean(contents);
            }
            return false;
        }
    }
}
