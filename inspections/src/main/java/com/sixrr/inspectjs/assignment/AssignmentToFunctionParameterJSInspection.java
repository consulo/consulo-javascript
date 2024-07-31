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
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class AssignmentToFunctionParameterJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.assignmentToFunctionParameterDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME.get();
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
        public void visitJSAssignmentExpression(JSAssignmentExpression jsAssignmentExpression) {
            super.visitJSAssignmentExpression(jsAssignmentExpression);
            final JSExpression lhs = jsAssignmentExpression.getLOperand();
            checkOperand(lhs);
        }

        @Override
        public void visitJSPrefixExpression(JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            final IElementType sign = expression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) && !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            final JSExpression operand = expression.getExpression();
            checkOperand(operand);
        }

        @Override
        public void visitJSPostfixExpression(JSPostfixExpression jsPostfixExpression) {
            super.visitJSPostfixExpression(jsPostfixExpression);
            final IElementType sign = jsPostfixExpression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) && !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            final JSExpression operand = jsPostfixExpression.getExpression();
            checkOperand(operand);
        }

        private void checkOperand(JSExpression operand) {
            if (operand instanceof JSDefinitionExpression definitionExpression
                && definitionExpression.getExpression() instanceof JSReferenceExpression referenceExpression
                && referenceExpression.resolve() instanceof JSParameter) {
                registerError(operand);
            }
            if (operand instanceof JSReferenceExpression referenceExpression
                && referenceExpression.resolve() instanceof JSParameter) {
                registerError(operand);
            }
        }
    }
}
