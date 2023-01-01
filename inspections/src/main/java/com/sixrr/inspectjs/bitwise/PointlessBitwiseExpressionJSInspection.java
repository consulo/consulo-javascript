package com.sixrr.inspectjs.bitwise;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.util.IncorrectOperationException;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.ui.SingleCheckboxOptionsPanel;
import com.sixrr.inspectjs.utils.ExpressionUtil;
import consulo.language.ast.IElementType;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.project.Project;

import javax.annotation.Nonnull;

import javax.swing.*;
import java.util.HashSet;
import java.util.Set;

@ExtensionImpl
public class PointlessBitwiseExpressionJSInspection extends JavaScriptInspection {

    /**
     * @noinspection PublicField
     */
    public boolean m_ignoreExpressionsContainingConstants = false;

    static final Set<IElementType> bitwiseTokens =
            new HashSet<IElementType>(6);

    static {
        bitwiseTokens.add(JSTokenTypes.AND);
        bitwiseTokens.add(JSTokenTypes.OR);
        bitwiseTokens.add(JSTokenTypes.XOR);
        bitwiseTokens.add(JSTokenTypes.LTLT);
        bitwiseTokens.add(JSTokenTypes.GTGT);
        bitwiseTokens.add(JSTokenTypes.GTGTGT);
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "pointless.bitwise.expression.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BITWISE_GROUP_NAME;
    }

    @Override
	@Nonnull
    public String buildErrorString(Object... args) {
        final String replacementExpression =
                calculateReplacementExpression((JSExpression) args[0]);
        return InspectionJSBundle.message(
                "pointless.bitwise.expression.problem.descriptor",
                replacementExpression);
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @Override
	public JComponent createOptionsPanel() {
        return new SingleCheckboxOptionsPanel(
                InspectionJSBundle.message(
                        "pointless.bitwise.expression.ignore.option"),
                this, "m_ignoreExpressionsContainingConstants");
    }

    String calculateReplacementExpression(JSExpression expression) {
        final JSBinaryExpression exp = (JSBinaryExpression) expression;
        final JSExpression lhs = exp.getLOperand();
        final JSExpression rhs = exp.getROperand();
        final IElementType tokenType = exp.getOperationSign();
        assert rhs != null;
        if (tokenType.equals(JSTokenTypes.AND)) {
            if (isZero(lhs) || isAllOnes(rhs)) {
                return lhs.getText();
            } else {
                return rhs.getText();
            }
        } else if (tokenType.equals(JSTokenTypes.OR)) {
            if (isZero(lhs) || isAllOnes(rhs)) {
                return rhs.getText();
            } else {
                return lhs.getText();
            }
        } else if (tokenType.equals(JSTokenTypes.XOR)) {
            if (isAllOnes(lhs)) {
                return '~' + rhs.getText();
            } else if (isAllOnes(rhs)) {
                return '~' + lhs.getText();
            } else if (isZero(rhs)) {
                return lhs.getText();
            } else {
                return rhs.getText();
            }
        } else if (tokenType.equals(JSTokenTypes.LTLT) ||
                tokenType.equals(JSTokenTypes.GTGT) ||
                tokenType.equals(JSTokenTypes.GTGTGT)) {
            return lhs.getText();
        } else {
            return "";
        }
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new PointlessBitwiseVisitor();
    }

    @Override
	public InspectionJSFix buildFix(PsiElement location) {
        return new PointlessBitwiseFix();
    }

    private class PointlessBitwiseFix extends InspectionJSFix {

        @Override
		@Nonnull
        public String getName() {
            return InspectionJSBundle.message(
                    "pointless.bitwise.expression.simplify.quickfix");
        }

        @Override
		public void doFix(Project project, ProblemDescriptor descriptor)
                throws IncorrectOperationException {
            final JSExpression expression = (JSExpression) descriptor
                    .getPsiElement();
            final String newExpression =
                    calculateReplacementExpression(expression);
            replaceExpression(expression, newExpression);
        }
    }

    private class PointlessBitwiseVisitor extends BaseInspectionVisitor {

        @Override public void visitJSBinaryExpression(
                @Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            final IElementType sign = expression.getOperationSign();
            if (!bitwiseTokens.contains(sign)) {
                return;
            }

            final JSExpression rhs = expression.getROperand();
            if (rhs == null) {
                return;
            }

            final JSExpression lhs = expression.getLOperand();

            final boolean isPointless;
            if (JSTokenTypes.AND.equals(sign)) {
                isPointless = andExpressionIsPointless(lhs, rhs);
            } else if (JSTokenTypes.OR.equals(sign)) {
                isPointless = orExpressionIsPointless(lhs, rhs);
            } else if (JSTokenTypes.XOR.equals(sign)) {
                isPointless = xorExpressionIsPointless(lhs, rhs );
            } else if (JSTokenTypes.LTLT.equals(sign) ||
                    JSTokenTypes.GTGT.equals(sign) ||
                    JSTokenTypes.GTGTGT.equals(sign)) {
                isPointless = shiftExpressionIsPointless(rhs);
            } else {
                isPointless = false;
            }
            if (isPointless) {
                registerError(expression, expression);
            }
        }

        private boolean andExpressionIsPointless(JSExpression lhs,
                                                 JSExpression rhs) {
            return isZero(lhs) || isZero(rhs)
                    || isAllOnes(lhs) || isAllOnes(rhs);
        }

        private boolean orExpressionIsPointless(JSExpression lhs,
                                                JSExpression rhs  ) {
            return isZero(lhs) || isZero(rhs)
                    || isAllOnes(lhs) || isAllOnes(rhs    );
        }

        private boolean xorExpressionIsPointless(JSExpression lhs,
                                                 JSExpression rhs ) {
            return isZero(lhs) || isZero(rhs)
                    || isAllOnes(lhs) || isAllOnes(rhs);
        }

        private boolean shiftExpressionIsPointless(JSExpression rhs
        ) {
            return isZero(rhs);
        }
    }

    private boolean isZero(JSExpression expression) {
        if (m_ignoreExpressionsContainingConstants
                && !(expression instanceof JSLiteralExpression)) {
            return false;
        }
        final Object value =
                ExpressionUtil.computeConstantExpression(expression);
        return value instanceof Integer && (Integer) value == 0;
    }

    private boolean isAllOnes(JSExpression expression) {
        if (m_ignoreExpressionsContainingConstants
                && !(expression instanceof JSLiteralExpression)) {
            return false;
        }
        final Object value =
                ExpressionUtil.computeConstantExpression(expression);
        return value != null && value instanceof Integer && (Integer) value == 0xffffffff;
    }
}