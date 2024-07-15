package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class IfStatementWithIdenticalBranchesJSInspection extends JavaScriptInspection {
    private InspectionJSFix fix = new CollapseIfFix();

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.ifStatementWithIdenticalBranchesDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.refStatementWithIdenticalBranchesErrorString().get();
    }

    @Override
    public InspectionJSFix buildFix(PsiElement location, Object state) {
        return fix;
    }

    private static class CollapseIfFix extends InspectionJSFix {
        @Override
        @Nonnull
        public String getName() {
            return InspectionJSLocalize.collapseIfStatementFix().get();
        }

        @Override
        public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException {
            final PsiElement identifier = descriptor.getPsiElement();
            final JSIfStatement statement = (JSIfStatement) identifier.getParent();
            assert statement != null;
            final JSStatement thenBranch = statement.getThen();
            final String bodyText = thenBranch.getText();
            replaceStatement(statement, bodyText);
        }
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new IfStatementWithIdenticalBranchesVisitor();
    }

    private static class IfStatementWithIdenticalBranchesVisitor extends BaseInspectionVisitor {

        @Override public void visitJSIfStatement(@Nonnull JSIfStatement statement) {
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