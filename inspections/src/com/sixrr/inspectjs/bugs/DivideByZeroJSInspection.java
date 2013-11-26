package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.tree.IElementType;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class DivideByZeroJSInspection extends JavaScriptInspection {

    @NotNull
    public String getID() {
        return "DivideByZeroJS";
    }

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("divide.by.zero.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @NotNull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("division.by.zero.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new DivisionByZeroVisitor();
    }

    private static class DivisionByZeroVisitor extends BaseInspectionVisitor {

        @Override public void visitJSBinaryExpression(
                @NotNull JSBinaryExpression expression) {
            super.visitJSBinaryExpression(expression);
            final JSExpression rhs = expression.getROperand();
            if (rhs == null) {
                return;
            }
            final IElementType tokenType = expression.getOperationSign();
            if (!JSTokenTypes.DIV.equals(tokenType) &&
                    !JSTokenTypes.PERC.equals(tokenType)) {
                return;
            }
            if(!isZero(rhs))
            {
                return;
            }
            registerError(expression);
        }

        @Override public void visitJSAssignmentExpression(
                JSAssignmentExpression expression) {
            super.visitJSAssignmentExpression(expression);
            final JSExpression rhs = expression.getROperand();
            if (rhs == null) {
                return;
            }
            final IElementType tokenType = expression.getOperationSign();
            if (!JSTokenTypes.DIVEQ.equals(tokenType)
                    && !JSTokenTypes.PERCEQ.equals(tokenType)) {
                return;
            }
            if (!isZero(rhs)) {
                return;
            }
            registerError(expression);
        }

    }

    private static boolean isZero(JSExpression expression) {
        @NonNls
        final String text = expression.getText();
        return "0".equals(text) ||
                "0x0".equals(text) ||
                "0X0".equals(text) ||
                "0.0".equals(text) ||
                "0L".equals(text) ||
                "0l".equals(text);
    }

}