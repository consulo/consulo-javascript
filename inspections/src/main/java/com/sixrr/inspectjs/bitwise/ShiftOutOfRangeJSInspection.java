package com.sixrr.inspectjs.bitwise;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ExpressionUtil;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ShiftOutOfRangeJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.shiftOperationByInappropriateConstantDisplayName();
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
        Integer value = (Integer)args[0];
        return value > 0
            ? InspectionJSLocalize.shiftOperationByInappropriateConstantProblemDescriptorTooLarge().get()
            : InspectionJSLocalize.shiftOperationByInappropriateConstantProblemDescriptorNegative().get();
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new ShiftOutOfRange();
    }

    private static class ShiftOutOfRange extends BaseInspectionVisitor {
        @Override
        public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            IElementType tokenType = expression.getOperationSign();
            if (tokenType == null || (!JSTokenTypes.LTLT.equals(tokenType)
                && !JSTokenTypes.GTGT.equals(tokenType) && !JSTokenTypes.GTGTGT.equals(tokenType))) {
                return;
            }
            JSExpression rhs = expression.getROperand();
            if (rhs == null) {
                return;
            }
            if (!ExpressionUtil.isConstantExpression(rhs)) {
                return;
            }
            Object valueObject = ExpressionUtil.computeConstantExpression(rhs);
            if (!(valueObject instanceof Integer)) {
                return;
            }
            int value = (Integer)valueObject;
            if (value < 0 || value > 31) {
                registerError(expression, value);
            }
        }
    }
}