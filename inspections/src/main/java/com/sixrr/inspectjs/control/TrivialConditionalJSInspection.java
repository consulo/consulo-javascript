package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.BoolUtils;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

@ExtensionImpl
public class TrivialConditionalJSInspection
        extends JavaScriptInspection {
    private final TrivialConditionalFix fix = new TrivialConditionalFix();

    @Override
	@Nonnull
    public String getID() {
        return "RedundantConditionalExpressionJS";
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("redundant.conditional.expression.display.name");
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

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new UnnecessaryConditionalExpressionVisitor();
    }

    @Override
	public String buildErrorString(Object... args) {
        final JSConditionalExpression exp = (JSConditionalExpression) args[0];
        return InspectionJSBundle.message("trivial.conditional.error.string", exp.getText(), calculateReplacementExpression(exp));
    }

    private static String calculateReplacementExpression(JSConditionalExpression exp) {
        final JSExpression thenExpression = exp.getThen();
        final JSExpression elseExpression = exp.getElse();
        final JSExpression condition = exp.getCondition();

        if (isFalse(thenExpression) && isTrue(elseExpression)) {
            return BoolUtils.getNegatedExpressionText(condition);
        } else {
            return condition.getText();
        }
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class
            TrivialConditionalFix extends InspectionJSFix {
        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message("simplify.fix");
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final JSConditionalExpression expression = (JSConditionalExpression) descriptor.getPsiElement();
            final String newExpression = calculateReplacementExpression(expression);
            replaceExpression(expression, newExpression);
        }
    }

    private static class UnnecessaryConditionalExpressionVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSConditionalExpression(JSConditionalExpression exp) {
            super.visitJSConditionalExpression(exp);
            final JSExpression thenExpression = exp.getThen();
            if (thenExpression == null) {
                return;
            }
            final JSExpression elseExpression = exp.getElse();
            if (elseExpression == null) {
                return;
            }
            if (((isFalse(thenExpression) && isTrue(elseExpression))
                    || (isTrue(thenExpression) && isFalse(elseExpression))) &&
                "Boolean".equals(JSResolveUtil.getExpressionType(exp.getCondition(), exp.getContainingFile())) 
               ) {
                registerError(exp);
            }
        }
    }

    private static boolean isFalse(JSExpression expression) {
        @NonNls final String text = expression.getText();
        return "false".equals(text);
    }

    private static boolean isTrue(JSExpression expression) {
        @NonNls final String text = expression.getText();
        return "true".equals(text);
    }
}
