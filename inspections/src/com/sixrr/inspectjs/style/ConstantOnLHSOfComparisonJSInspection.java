package com.sixrr.inspectjs.style;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.ComparisonUtils;
import org.jetbrains.annotations.NotNull;

public class ConstantOnLHSOfComparisonJSInspection extends JavaScriptInspection {
    private final SwapComparisonFix fix = new SwapComparisonFix();

    @Override
	@NotNull
    public String getID() {
        return "ConstantOnLefSideOfComparisonJS";
    }

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("constant.on.left.side.of.comparison.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME;
    }

    @Override
	public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("constant.on.left.side.of.comparison.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new ConstantOnRHSOfComparisonVisitor();
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class SwapComparisonFix extends InspectionJSFix {
        @Override
		@NotNull
        public String getName() {
            return InspectionJSBundle.message("flip.comparison.fix");
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final JSBinaryExpression expression = (JSBinaryExpression) descriptor.getPsiElement();
            final JSExpression rhs = expression.getROperand();
            final JSExpression lhs = expression.getLOperand();
            final IElementType sign = expression.getOperationSign();
            assert rhs != null;
            final String rhsText = rhs.getText();
            final String flippedComparison = ComparisonUtils.getFlippedComparison(sign);
            final String lhsText = lhs.getText();
            replaceExpression(expression,
                    rhsText + ' ' + flippedComparison + ' ' + lhsText);
        }
    }

    private static class ConstantOnRHSOfComparisonVisitor extends BaseInspectionVisitor {

        @Override public void visitJSBinaryExpression(@NotNull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            if (!(expression.getROperand() != null)) {
                return;
            }
            if (!ComparisonUtils.isComparison(expression)) {
                return;
            }
            final JSExpression lhs = expression.getLOperand();
            final JSExpression rhs = expression.getROperand();
            if (!(lhs instanceof JSLiteralExpression) ||
                    rhs instanceof JSLiteralExpression) {
                return;
            }
            registerError(expression);
        }
    }
}
