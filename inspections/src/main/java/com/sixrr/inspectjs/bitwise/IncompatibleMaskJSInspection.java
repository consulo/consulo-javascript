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
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.lang.annotations.Pattern;

@ExtensionImpl
public class IncompatibleMaskJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "IncompatibleBitwiseMaskOperation";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.incompatibleMaskOperationDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.BITWISE_GROUP_NAME;
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        JSBinaryExpression binaryExpression = (JSBinaryExpression) args[0];
        IElementType tokenType = binaryExpression.getOperationSign();
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
        @RequiredReadAction
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            JSExpression rhs = expression.getROperand();
            if (!ComparisonUtils.isEqualityComparison(expression)) {
                return;
            }
            JSExpression strippedRhs = ParenthesesUtils.stripParentheses(rhs);
            if (strippedRhs == null) {
                return;
            }
            JSExpression lhs = expression.getLOperand();
            JSExpression strippedLhs = ParenthesesUtils.stripParentheses(lhs);
            if (strippedLhs == null) {
                return;
            }
            if (isConstantMask(strippedLhs) && ExpressionUtil.isConstantExpression(strippedRhs)) {
                if (isIncompatibleMask((JSBinaryExpression) strippedLhs, strippedRhs)) {
                    registerError(expression, expression);
                }
            }
            else if (isConstantMask(strippedRhs) && ExpressionUtil.isConstantExpression(strippedLhs)) {
                if (isIncompatibleMask((JSBinaryExpression) strippedRhs, strippedLhs)) {
                    registerError(expression, expression);
                }
            }
        }

        @RequiredReadAction
        private static boolean isIncompatibleMask(JSBinaryExpression maskExpression, JSExpression constantExpression) {
            IElementType tokenType = maskExpression.getOperationSign();
            Object constantValue = ExpressionUtil.computeConstantExpression(constantExpression);
            if (!(constantValue instanceof Integer)) {
                return false;
            }
            int constantLongValue = (Integer) constantValue;
            JSExpression maskRhs = maskExpression.getROperand();
            JSExpression maskLhs = maskExpression.getLOperand();
            int constantMaskValue;
            if (ExpressionUtil.isConstantExpression(maskRhs)) {
                Object rhsValue = ExpressionUtil.computeConstantExpression(maskRhs);
                if (!(rhsValue instanceof Integer)) {
                    return false; // Might indeed be the case with "null" literal
                    // whoes constant value evaluates to null. Check out (a|null) case.
                }
                constantMaskValue = (Integer) rhsValue;
            }
            else {
                Object lhsValue = ExpressionUtil.computeConstantExpression(maskLhs);
                if (!(lhsValue instanceof Integer)) {
                    return false;
                }
                constantMaskValue = (Integer) lhsValue;
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

        @RequiredReadAction
        private static boolean isConstantMask(JSExpression expression) {
            if (expression == null) {
                return false;
            }
            if (!(expression instanceof JSBinaryExpression binaryExpression)) {
                return false;
            }
            IElementType tokenType = binaryExpression.getOperationSign();
            if (!JSTokenTypes.OR.equals(tokenType) && !JSTokenTypes.AND.equals(tokenType)) {
                return false;
            }
            JSExpression rhs = binaryExpression.getROperand();
            if (ExpressionUtil.isConstantExpression(rhs)) {
                return true;
            }
            JSExpression lhs = binaryExpression.getLOperand();
            return ExpressionUtil.isConstantExpression(lhs);
        }
    }
}
