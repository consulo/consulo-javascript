package com.sixrr.inspectjs.control;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.BoolUtils;
import org.jetbrains.annotations.NotNull;

public class ConstantIfStatementJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "constant.if.statement.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    public boolean isEnabledByDefault() {
        return true;
    }

    @NotNull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message(
                "constant.if.statement.problem.descriptor");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new ConstantIfStatementVisitor();
    }

    public InspectionJSFix buildFix(PsiElement location) {
        return new ConstantIfStatementFix();
    }

    private static class ConstantIfStatementFix extends InspectionJSFix {

        @NotNull
        public String getName() {
            return InspectionJSBundle.message(
                    "constant.conditional.expression.simplify.quickfix");
        }

        public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final PsiElement ifKeyword = descriptor.getPsiElement();
            final JSIfStatement statement =
                    (JSIfStatement) ifKeyword.getParent();
            assert statement != null;
            final JSStatement thenBranch = statement.getThen();
            final JSStatement elseBranch = statement.getElse();
            final JSExpression condition = statement.getCondition();
            if (BoolUtils.isFalse(condition)) {
                if (elseBranch != null) {
                    replaceStatementWithUnwrapping(elseBranch, statement);
                } else {
                    deleteElement(statement);
                }
            } else {
                replaceStatementWithUnwrapping(thenBranch, statement);
            }
        }

        private static void replaceStatementWithUnwrapping(
                JSStatement branch, JSIfStatement statement)
                throws IncorrectOperationException {
            {
                final String elseText = branch.getText();
                replaceStatement(statement, elseText);
            }
        }
    }

    private static class ConstantIfStatementVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSIfStatement(JSIfStatement statement) {
            super.visitJSIfStatement(statement);
            final JSExpression condition = statement.getCondition();
            if (condition == null) {
                return;
            }
            final JSStatement thenBranch = statement.getThen();
            if (thenBranch == null) {
                return;
            }
            if (BoolUtils.isTrue(condition) || BoolUtils.isFalse(condition)) {
                registerStatementError(statement);
            }
        }
    }
}