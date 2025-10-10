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
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class AssignmentToForLoopParameterJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.assignmentToForLoopParameterDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.assignmentToForLoopParameterErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        @RequiredReadAction
        public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression expression) {
            super.visitJSAssignmentExpression(expression);
            JSExpression rhs = expression.getROperand();
            if (rhs == null) {
                return;
            }
            JSExpression lhs = expression.getLOperand();
            checkForForLoopParam(lhs);
            checkForForeachLoopParam(lhs);
        }

        @Override
        @RequiredReadAction
        public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            IElementType sign = expression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) &&
                !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            JSExpression operand = expression.getExpression();
            if (operand == null) {
                return;
            }
            checkForForLoopParam(operand);
            checkForForeachLoopParam(operand);
        }

        @Override
        @RequiredReadAction
        public void visitJSPostfixExpression(@Nonnull JSPostfixExpression expression) {
            super.visitJSPostfixExpression(expression);
            IElementType sign = expression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) && !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            JSExpression operand = expression.getExpression();
            if (operand == null) {
                return;
            }
            checkForForLoopParam(operand);
            checkForForeachLoopParam(operand);
        }

        @RequiredReadAction
        private void checkForForLoopParam(JSExpression expression) {
            if (expression instanceof JSReferenceExpression ref
                && ref.resolve() instanceof JSVariable variable
                && variable.getParent() instanceof JSVarStatement decl
                && decl.getParent() instanceof JSForStatement forStmt
                && isInForStatementBody(expression, forStmt)) {
                registerError(expression);
            }
        }

        @RequiredReadAction
        private void checkForForeachLoopParam(JSExpression expression) {
            if (expression instanceof JSReferenceExpression ref
                && ref.resolve() instanceof JSVariable parameter
                && parameter.getParent().getParent() instanceof JSForInStatement) {
                registerError(expression);
            }
        }

        @RequiredReadAction
        private static boolean isInForStatementBody(JSExpression expression, JSForStatement statement) {
            JSStatement body = statement.getBody();
            return PsiTreeUtil.isAncestor(body, expression, true);
        }
    }
}
