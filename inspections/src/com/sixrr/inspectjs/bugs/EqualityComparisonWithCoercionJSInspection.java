package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.PsiElement;
import com.intellij.openapi.project.Project;
import com.intellij.codeInspection.ProblemDescriptor;
import com.intellij.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class EqualityComparisonWithCoercionJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getID() {
        return "EqualityComparisonWithCoercionJS";
    }

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("equality.comparison.with.coercion.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Override
	@NotNull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("equality.comparison.with.coercion.error.string");
    }

	@Override
	protected InspectionJSFix buildFix(PsiElement location) {
		final JSBinaryExpression expression = (JSBinaryExpression) location;
		final IElementType sign = expression.getOperationSign();
		if (JSTokenTypes.EQEQ == sign) {
			return new EqualityComparisonWithCoercionFix("===");
		} else if (JSTokenTypes.NE == sign) {
			return new EqualityComparisonWithCoercionFix("!==");
		}
		return null;
	}

	private static class EqualityComparisonWithCoercionFix extends InspectionJSFix {

		private final String sign;

		public EqualityComparisonWithCoercionFix(String sign) {
			this.sign = sign;
		}

		@NotNull
		public String getName() {
			return InspectionJSBundle.message("equality.comparison.with.coercion.fix", sign);
		}

		@Override
		protected void doFix(Project project, ProblemDescriptor descriptor)
				throws IncorrectOperationException {
			final JSBinaryExpression expression = (JSBinaryExpression) descriptor.getPsiElement();
			final JSExpression lhs = expression.getLOperand();
			final JSExpression rhs = expression.getROperand();
			replaceExpression(expression, lhs.getText() + sign + rhs.getText());
		}
	}

	@Override
	public BaseInspectionVisitor buildVisitor() {
        return new EqualityComparisonWithCoercionVisitor();
    }

    private static class EqualityComparisonWithCoercionVisitor extends BaseInspectionVisitor {

        @Override public void visitJSBinaryExpression(
                @NotNull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            final JSExpression lhs = expression.getLOperand();
            if (lhs == null) {
                return;
            }
            final JSExpression rhs = expression.getROperand();
            if (rhs == null) {
                return;
            }
            final IElementType tokenType = expression.getOperationSign();
            if (!JSTokenTypes.EQEQ.equals(tokenType) &&
                    !JSTokenTypes.NE.equals(tokenType)) {
                return;
            }
            if(!mayCauseCoercion(rhs) &&!mayCauseCoercion(lhs)) {
				return;
            }
            registerError(expression);
        }
    }

    private static boolean mayCauseCoercion(JSExpression expression) {
        @NonNls
        final String text = expression.getText();
        return "0".equals(text) ||
                "0x0".equals(text) ||
                "0X0".equals(text) ||
                "0.0".equals(text) ||
                "0L".equals(text) ||
                "0l".equals(text) ||
                "null".equals(text) ||
                "undefined".equals(text) ||
                "true".equals(text) ||
                "false".equals(text) ||
                "''".equals(text);
    }
}
