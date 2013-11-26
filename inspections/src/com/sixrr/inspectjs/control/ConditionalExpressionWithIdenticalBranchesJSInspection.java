package com.sixrr.inspectjs.control;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import org.jetbrains.annotations.NotNull;

public class ConditionalExpressionWithIdenticalBranchesJSInspection extends JavaScriptInspection {
    private InspectionJSFix fix = new CollapseConditional();

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("conditional.expression.with.identical.branches.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("conditional.expression.with.identical.branches.error.string");
    }

    public InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class CollapseConditional extends InspectionJSFix {
        @NotNull
        public String getName() {
            return InspectionJSBundle.message("collapse.conditional.expression.fix");
        }

        public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final JSConditionalExpression expression =
                    (JSConditionalExpression) descriptor.getPsiElement();

            final JSExpression thenExpression = expression.getThen();
            assert thenExpression != null;
            final String bodyText = thenExpression.getText();
            replaceExpression(expression, bodyText);
        }
    }

    public BaseInspectionVisitor buildVisitor() {
        return new ConditionalExpressionWithIdenticalBranchesVisitor();
    }

    private static class ConditionalExpressionWithIdenticalBranchesVisitor extends BaseInspectionVisitor {

        @Override public void visitJSConditionalExpression(JSConditionalExpression expression) {
            super.visitJSConditionalExpression(expression);
            final JSExpression thenExpression = expression.getThen();
            final JSExpression elseExpression = expression.getElse();
            if (EquivalenceChecker.expressionsAreEquivalent(thenExpression, elseExpression)) {
                registerError(expression);
            }
        }
    }
}