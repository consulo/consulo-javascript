package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ConditionalExpressionWithIdenticalBranchesJSInspection extends JavaScriptInspection {
    private InspectionJSFix fix = new CollapseConditional();

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("conditional.expression.with.identical.branches.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
	public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("conditional.expression.with.identical.branches.error.string");
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class CollapseConditional extends InspectionJSFix {
        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message("collapse.conditional.expression.fix");
        }

        @Override
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

    @Override
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