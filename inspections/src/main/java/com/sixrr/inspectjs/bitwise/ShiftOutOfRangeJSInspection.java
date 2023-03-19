package com.sixrr.inspectjs.bitwise;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.utils.ExpressionUtil;
import javax.annotation.Nonnull;

@ExtensionImpl
public class ShiftOutOfRangeJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "shift.operation.by.inappropriate.constant.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BITWISE_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nonnull
    public String buildErrorString(Object state, Object... args) {
        final Integer value = (Integer) args[0];
        if (value > 0) {
            return InspectionJSBundle.message(
                    "shift.operation.by.inappropriate.constant.problem.descriptor.too.large");
        } else {
            return InspectionJSBundle.message(
                    "shift.operation.by.inappropriate.constant.problem.descriptor.negative");
        }
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

        @Override public void visitJSBinaryExpression(
                @Nonnull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            final IElementType tokenType = expression.getOperationSign();
            if (tokenType == null ||
                  ( !tokenType.equals(JSTokenTypes.LTLT) &&
                    !tokenType.equals(JSTokenTypes.GTGT) &&
                    !tokenType.equals(JSTokenTypes.GTGTGT) )) {
                return;
            }
            final JSExpression rhs = expression.getROperand();
            if (rhs == null) {
                return;
            }
            if (!ExpressionUtil.isConstantExpression(rhs)) {
                return;
            }
            final Object valueObject =
                     ExpressionUtil.computeConstantExpression(rhs);
            if (!(valueObject instanceof Integer)) {
                return;
            }
            final int value = (Integer) valueObject;
            if (value < 0 || value > 31) {
                registerError(expression, value);
            }
        }
    }
}