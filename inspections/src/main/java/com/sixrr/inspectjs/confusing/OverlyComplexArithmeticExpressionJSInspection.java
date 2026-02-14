package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

import java.util.Set;

@ExtensionImpl
public class OverlyComplexArithmeticExpressionJSInspection extends JavaScriptInspection {
    protected static final Set<IElementType> OUR_ARITHMETIC_TOKENS = Set.of(
        JSTokenTypes.PLUS,
        JSTokenTypes.MINUS,
        JSTokenTypes.MULT,
        JSTokenTypes.DIV,
        JSTokenTypes.PERC
    );

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.overlyComplexArithmeticExpressionDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new OverlyComplexArithmeticExpressionJSInspectionState();
    }

    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.overlyComplexArithmeticExpressionErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<OverlyComplexArithmeticExpressionJSInspectionState> {
        @Override
        @RequiredReadAction
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            checkExpression(expression);
        }

        @Override
        @RequiredReadAction
        public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            checkExpression(expression);
        }

        @Override
        @RequiredReadAction
        public void visitJSParenthesizedExpression(@Nonnull JSParenthesizedExpression expression) {
            super.visitJSParenthesizedExpression(expression);
            checkExpression(expression);
        }

        @RequiredReadAction
        private void checkExpression(JSExpression expression) {
            if (isParentArithmetic(expression)) {
                return;
            }
            if (!isArithmetic(expression)) {
                return;
            }
            if (containsStringConcatenation(expression)) {
                return;
            }
            int numTerms = countTerms(expression);
            if (numTerms <= myState.myMLimit) {
                return;
            }
            registerError(expression);
        }

        @RequiredReadAction
        private int countTerms(JSExpression expression) {
            if (expression == null) {
                return 0;
            }
            if (!isArithmetic(expression)) {
                return 1;
            }
            if (expression instanceof JSBinaryExpression binaryExpr) {
                return countTerms(binaryExpr.getLOperand()) + countTerms(binaryExpr.getROperand());
            }
            else if (expression instanceof JSPrefixExpression prefixExpr) {
                return countTerms(prefixExpr.getExpression());
            }
            else if (expression instanceof JSParenthesizedExpression parenthesized) {
                return countTerms(parenthesized.getInnerExpression());
            }
            return 1;
        }

        @RequiredReadAction
        private boolean isParentArithmetic(JSExpression expression) {
            return expression.getParent() instanceof JSExpression parentExpr && isArithmetic(parentExpr);
        }

        @RequiredReadAction
        private boolean isArithmetic(JSExpression expression) {
            if (expression instanceof JSBinaryExpression binaryExpr) {
                IElementType sign = binaryExpr.getOperationSign();
                return sign != null && OUR_ARITHMETIC_TOKENS.contains(sign);
            }
            else if (expression instanceof JSPrefixExpression prefixExpr) {
                IElementType sign = prefixExpr.getOperationSign();
                return sign != null && OUR_ARITHMETIC_TOKENS.contains(sign);
            }
            else if (expression instanceof JSParenthesizedExpression parenthesizedExpression) {
                JSExpression contents = parenthesizedExpression.getInnerExpression();
                return isArithmetic(contents);
            }
            return false;
        }

        @RequiredReadAction
        private boolean containsStringConcatenation(JSExpression expression) {
            if (isStringLiteral(expression)) {
                return true;
            }
            if (expression instanceof JSBinaryExpression binaryExpr) {
                return containsStringConcatenation(binaryExpr.getLOperand())
                    || containsStringConcatenation(binaryExpr.getROperand());
            }
            else if (expression instanceof JSPrefixExpression prefixExpr) {
                IElementType sign = prefixExpr.getOperationSign();
                return sign != null && OUR_ARITHMETIC_TOKENS.contains(sign);
            }
            else if (expression instanceof JSParenthesizedExpression parenthesized) {
                return containsStringConcatenation(parenthesized.getInnerExpression());
            }
            return false;
        }

        @RequiredReadAction
        private boolean isStringLiteral(JSExpression expression) {
            if (expression instanceof JSLiteralExpression literal) {
                String text = literal.getText();
                return text.startsWith("'") || text.startsWith("\"");
            }
            if (expression instanceof JSReferenceExpression reference
                && reference.resolve() instanceof JSVariable variable && variable.isConst()) {
                JSExpression initializer = variable.getInitializer();
                if (initializer != null) {
                    String text = initializer.getText();
                    return text.startsWith("'") || text.startsWith("\"");
                }
            }
            return false;
        }
    }
}
