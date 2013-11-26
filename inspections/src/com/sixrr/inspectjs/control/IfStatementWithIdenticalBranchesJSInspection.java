package com.sixrr.inspectjs.control;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import org.jetbrains.annotations.NotNull;

public class IfStatementWithIdenticalBranchesJSInspection extends JavaScriptInspection {
    private InspectionJSFix fix = new CollapseIfFix();

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("if.statement.with.identical.branches.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("ref.statement.with.identical.branches.error.string");
    }

    public InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class CollapseIfFix extends InspectionJSFix {
        @NotNull
        public String getName() {
            return InspectionJSBundle.message("collapse.if.statement.fix");
        }

        public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final PsiElement identifier = descriptor.getPsiElement();
            final JSIfStatement statement =
                    (JSIfStatement) identifier.getParent();
            assert statement != null;
            final JSStatement thenBranch = statement.getThen();
            final String bodyText = thenBranch.getText();
            replaceStatement(statement, bodyText);
        }
    }

    public BaseInspectionVisitor buildVisitor() {
        return new IfStatementWithIdenticalBranchesVisitor();
    }

    private static class IfStatementWithIdenticalBranchesVisitor extends BaseInspectionVisitor {

        @Override public void visitJSIfStatement(@NotNull JSIfStatement statement) {
            super.visitJSIfStatement(statement);
            final JSStatement thenBranch = statement.getThen();
            final JSStatement elseBranch = statement.getElse();
            if(thenBranch == null || elseBranch == null)
            {
                return ;
            }
            if (EquivalenceChecker.statementsAreEquivalent(thenBranch, elseBranch)) {
                registerStatementError(statement);
            }
        }
    }
}