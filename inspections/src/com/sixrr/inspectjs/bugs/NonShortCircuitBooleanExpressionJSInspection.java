package com.sixrr.inspectjs.bugs;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class NonShortCircuitBooleanExpressionJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("non.short.circuit.boolean.expression.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("non.short.circuit.boolean.expression.error.string");
    }

    public InspectionJSFix buildFix(PsiElement location) {
        return new NonShortCircuitBooleanFix();
    }

    private static class NonShortCircuitBooleanFix
            extends InspectionJSFix {

        @NotNull
        public String getName() {
            return InspectionJSBundle.message("replace.with.short.circuit.expression.fix.string");
        }

        public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final JSBinaryExpression expression =
                    (JSBinaryExpression) descriptor.getPsiElement();
            final JSExpression lhs = expression.getLOperand();
            final JSExpression rhs = expression.getROperand();
            final IElementType operationSign = expression.getOperationSign();
            assert rhs != null;
            final String newExpression = lhs.getText() +
                    getShortCircuitOperand(operationSign) + rhs.getText();
            replaceExpression(expression, newExpression);
        }

        private static String getShortCircuitOperand(IElementType tokenType) {
            if (JSTokenTypes.AND.equals(tokenType)) {
                return "&&";
            } else {
                return "||";
            }
        }

    }

    public BaseInspectionVisitor buildVisitor() {
        return new NonShortCircuitBooleanVisitor();
    }

    private static class NonShortCircuitBooleanVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSBinaryExpression(
                @NotNull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            if (!(expression.getROperand() != null)) {
                return;
            }
            final IElementType sign = expression.getOperationSign();
            if (!JSTokenTypes.AND.equals(sign) &&
                    !JSTokenTypes.OR.equals(sign)) {
                return;
            }
            registerError(expression);
        }
    }
}