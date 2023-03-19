package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

@ExtensionImpl
public class DivideByZeroJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getID() {
        return "DivideByZeroJS";
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("divide.by.zero.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nonnull
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("division.by.zero.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new DivisionByZeroVisitor();
    }

    private static class DivisionByZeroVisitor extends BaseInspectionVisitor {

        @Override public void visitJSBinaryExpression(
                @Nonnull JSBinaryExpression expression) {
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