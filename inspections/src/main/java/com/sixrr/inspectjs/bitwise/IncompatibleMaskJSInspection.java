package com.sixrr.inspectjs.bitwise;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ComparisonUtils;
import com.sixrr.inspectjs.utils.ExpressionUtil;
import com.sixrr.inspectjs.utils.ParenthesesUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class IncompatibleMaskJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getID() {
        return "IncompatibleBitwiseMaskOperation";
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.incompatibleMaskOperationDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BITWISE_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    @Nonnull
    public String buildErrorString(Object state, Object... args) {
        final JSBinaryExpression binaryExpression = (JSBinaryExpression)args[0];
        final IElementType tokenType = binaryExpression.getOperationSign();
        if (JSTokenTypes.EQEQ.equals(tokenType)) {
            return InspectionJSLocalize.incompatibleMaskOperationProblemDescriptorAlwaysFalse().get();
        }
        else {
            return InspectionJSLocalize.incompatibleMaskOperationProblemDescriptorAlwaysTrue().get();
        }
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new IncompatibleMaskVisitor();
    }

    private static class IncompatibleMaskVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            final JSExpression rhs = expression.getROperand();
            if (!ComparisonUtils.isEqualityComparison(expression)) {
                return;
            }
            final JSExpression strippedRhs = ParenthesesUtils.stripParentheses(rhs);
            if (strippedRhs == null) {
                return;
            }
            final JSExpression lhs = expression.getLOperand();
            final JSExpression strippedLhs = ParenthesesUtils.stripParentheses(lhs);
            if (strippedLhs == null) {
                return;
            }
            if (isConstantMask(strippedLhs) && ExpressionUtil.isConstantExpression(strippedRhs)) {
                if (isIncompatibleMask((JSBinaryExpression)strippedLhs, strippedRhs)) {
                    registerError(expression, expression);
                }
            }
            else if (isConstantMask(strippedRhs) && ExpressionUtil.isConstantExpression(strippedLhs)) {
                if (isIncompatibleMask((JSBinaryExpression)strippedRhs, strippedLhs)) {
                    registerError(expression, expression);
                }
            }
        }

        private static boolean isIncompatibleMask(JSBinaryExpression maskExpression, JSExpression constantExpression) {
            final IElementType tokenType = maskExpression.getOperationSign();
            final Object constantValue = ExpressionUtil.computeConstantExpression(constantExpression);
            if (!(constantValue instanceof Integer)) {
                return false;
            }
            final int constantLongValue = (Integer)constantValue;
            final JSExpression maskRhs = maskExpression.getROperand();
            final JSExpression maskLhs = maskExpression.getLOperand();
            final int constantMaskValue;
            if (ExpressionUtil.isConstantExpression(maskRhs)) {
                final Object rhsValue =
                    ExpressionUtil.computeConstantExpression(maskRhs);
                if (!(rhsValue instanceof Integer)) {
                    return false; // Might indeed be the case with "null" literal
                    // whoes constant value evaluates to null. Check out (a|null) case.
                }
                constantMaskValue = (Integer)rhsValue;
            }
            else {
                final Object lhsValue =
                    ExpressionUtil.computeConstantExpression(maskLhs);
                if (!(lhsValue instanceof Integer)) {
                    return false;
                }
                constantMaskValue = (Integer)lhsValue;
            }

            if (JSTokenTypes.OR.equals(tokenType)) {
                if ((constantMaskValue | constantLongValue) != constantLongValue) {
                    return true;
                }
            }
            if (JSTokenTypes.AND.equals(tokenType)) {
                if ((constantMaskValue | constantLongValue) != constantMaskValue) {
                    return true;
                }
            }
            return false;
        }

        private static boolean isConstantMask(JSExpression expression) {
            if (expression == null) {
                return false;
            }
            if (!(expression instanceof JSBinaryExpression)) {
                return false;
            }
            final JSBinaryExpression binaryExpression = (JSBinaryExpression)expression;
            final IElementType tokenType = binaryExpression.getOperationSign();
            if (!JSTokenTypes.OR.equals(tokenType) && !JSTokenTypes.AND.equals(tokenType)) {
                return false;
            }
            final JSExpression rhs = binaryExpression.getROperand();
            if (ExpressionUtil.isConstantExpression(rhs)) {
                return true;
            }
            final JSExpression lhs = binaryExpression.getLOperand();
            return ExpressionUtil.isConstantExpression(lhs);
        }
    }
}
