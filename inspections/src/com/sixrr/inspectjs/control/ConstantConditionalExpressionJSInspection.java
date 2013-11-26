package com.sixrr.inspectjs.control;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.BoolUtils;
import org.jetbrains.annotations.NotNull;

public class ConstantConditionalExpressionJSInspection
        extends JavaScriptInspection {

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("constant.conditional.expression.display.name");
    }

    public boolean isEnabledByDefault() {
        return true;
    }

    public BaseInspectionVisitor buildVisitor() {
        return new ConstantConditionalExpressionVisitor();
    }

    @NotNull
    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("constant.conditional.expression.error.string");
    }

    static String calculateReplacementExpression(
            JSConditionalExpression exp) {
        final JSExpression thenExpression = exp.getThen();
        final JSExpression elseExpression = exp.getElse();
        final JSExpression condition = exp.getCondition();
        assert thenExpression != null;
        assert elseExpression != null;
        if (BoolUtils.isTrue(condition)) {
            return thenExpression.getText();
        } else {
            return elseExpression.getText();
        }
    }

    public InspectionJSFix buildFix(PsiElement location) {
        return new ConstantConditionalFix();
    }

    private static class ConstantConditionalFix extends InspectionJSFix {

        @NotNull
        public String getName() {
            return InspectionJSBundle.message("simplify.fix");
        }

        public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final JSConditionalExpression expression =
                    (JSConditionalExpression) descriptor.getPsiElement();
            final String newExpression =
                    calculateReplacementExpression(expression);
            replaceExpression(expression, newExpression);
        }
    }

    private static class ConstantConditionalExpressionVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSConditionalExpression(
                JSConditionalExpression expression) {
            super.visitJSConditionalExpression(expression);
            final JSExpression condition = expression.getCondition();
            final JSExpression thenExpression = expression.getThen();
            if (thenExpression == null) {
                return;
            }
            final JSExpression elseExpression = expression.getElse();
            if (elseExpression == null) {
                return;
            }
            if (BoolUtils.isFalse(condition) || BoolUtils.isTrue(condition)) {
                registerError(expression, expression);
            }
        }
    }
}
