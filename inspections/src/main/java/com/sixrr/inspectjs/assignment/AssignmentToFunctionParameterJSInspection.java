package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class AssignmentToFunctionParameterJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.assignmentToFunctionParameterDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.assignmentToFunctionParameterErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        @RequiredReadAction
        public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression jsAssignmentExpression) {
            super.visitJSAssignmentExpression(jsAssignmentExpression);
            JSExpression lhs = jsAssignmentExpression.getLOperand();
            checkOperand(lhs);
        }

        @Override
        @RequiredReadAction
        public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            IElementType sign = expression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) && !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            JSExpression operand = expression.getExpression();
            checkOperand(operand);
        }

        @Override
        @RequiredReadAction
        public void visitJSPostfixExpression(@Nonnull JSPostfixExpression postfixExpr) {
            super.visitJSPostfixExpression(postfixExpr);
            IElementType sign = postfixExpr.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) && !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            JSExpression operand = postfixExpr.getExpression();
            checkOperand(operand);
        }

        @RequiredReadAction
        private void checkOperand(JSExpression operand) {
            if (operand instanceof JSDefinitionExpression defExpr
                && defExpr.getExpression() instanceof JSReferenceExpression refExpr
                && refExpr.resolve() instanceof JSParameter) {
                registerError(operand);
            }
            if (operand instanceof JSReferenceExpression refExpr
                && refExpr.resolve() instanceof JSParameter) {
                registerError(operand);
            }
        }
    }
}
