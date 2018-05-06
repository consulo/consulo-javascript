package com.sixrr.inspectjs.confusing;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.BoolUtils;
import com.sixrr.inspectjs.utils.ParenthesesUtils;

public class NegatedConditionalExpressionJSInspection extends JavaScriptInspection {
    private final NegatedConditionalFix fix = new NegatedConditionalFix();

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("negated.conditional.expression.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("negated.conditional.expression.error.string");
    }

    @Override
	protected InspectionJSFix buildFix(PsiElement location) {
        return fix;
    }

    private static class NegatedConditionalFix extends InspectionJSFix {
        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message("invert.condition.fix");
        }

        @Override
		public void doFix(Project project,
                          ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final JSConditionalExpression exp =
                    (JSConditionalExpression) descriptor.getPsiElement();
            assert exp != null;
            final JSExpression elseBranch = (JSExpression) exp.getElse();
            final JSExpression thenBranch = exp.getThen();
            final JSExpression condition = exp.getCondition();
            final String negatedCondition =
                    BoolUtils.getNegatedExpressionText(condition);
            assert elseBranch != null;
            assert thenBranch != null;
            final String newStatement =
                    negatedCondition + '?' + elseBranch.getText() + ':' +
                            thenBranch.getText();
            replaceExpression(exp, newStatement);
        }
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSConditionalExpression(JSConditionalExpression exp) {
            super.visitJSConditionalExpression(exp);
            JSExpression condition = exp.getCondition();
            condition = ParenthesesUtils.stripExpression(condition);
            if (!BoolUtils.isNegation(condition) && !isNotEquals(condition)) {
                return;
            }
            registerError(exp);
        }

        private boolean isNotEquals(JSExpression expression) {
            if (!(expression instanceof JSBinaryExpression)) {
                return false;
            }
            final JSBinaryExpression binaryExpression =
                    (JSBinaryExpression) expression;
            final IElementType sign = binaryExpression.getOperationSign();
            return JSTokenTypes.NE.equals(sign) ||JSTokenTypes.NEQEQ.equals(sign);
        }
    }
}
