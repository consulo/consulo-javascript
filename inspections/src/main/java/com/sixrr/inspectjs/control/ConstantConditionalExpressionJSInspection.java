package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.project.Project;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.BoolUtils;
import consulo.language.editor.inspection.ProblemDescriptor;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ConstantConditionalExpressionJSInspection
        extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("constant.conditional.expression.display.name");
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new ConstantConditionalExpressionVisitor();
    }

    @RequiredReadAction
	@Override
	@Nonnull
    public String buildErrorString(Object state, Object... args) {
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

    @Override
	public InspectionJSFix buildFix(PsiElement location, Object state) {
        return new ConstantConditionalFix();
    }

    private static class ConstantConditionalFix extends InspectionJSFix {

        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message("simplify.fix");
        }

        @Override
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
