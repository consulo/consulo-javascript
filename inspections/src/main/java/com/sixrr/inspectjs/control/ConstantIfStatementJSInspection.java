package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.BoolUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ConstantIfStatementJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "constant.if.statement.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
	@Override
	@Nonnull
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message(
                "constant.if.statement.problem.descriptor");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new ConstantIfStatementVisitor();
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location, Object state) {
        return new ConstantIfStatementFix();
    }

    private static class ConstantIfStatementFix extends InspectionJSFix {

        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message(
                    "constant.conditional.expression.simplify.quickfix");
        }

        @Override
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