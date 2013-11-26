package com.sixrr.inspectjs.assignment;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;

public class AssignmentToForLoopParameterJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("assignment.to.for.loop.parameter.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.ASSIGNMENT_GROUP_NAME;
    }

    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("assignment.to.for.loop.parameter.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSAssignmentExpression(@NotNull JSAssignmentExpression expression) {
            super.visitJSAssignmentExpression(expression);
            final JSExpression rhs = expression.getROperand();
            if (rhs == null) {
                return;
            }
            final JSExpression lhs = expression.getLOperand();
            checkForForLoopParam(lhs);
            checkForForeachLoopParam(lhs);
        }

        @Override public void visitJSPrefixExpression(@NotNull JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            final IElementType sign = expression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) &&
                    !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            final JSExpression operand = expression.getExpression();
            if (operand == null) {
                return;
            }
            checkForForLoopParam(operand);
            checkForForeachLoopParam(operand);
        }

        @Override public void visitJSPostfixExpression(@NotNull JSPostfixExpression expression) {
            super.visitJSPostfixExpression(expression);
            final IElementType sign = expression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) &&
                    !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            final JSExpression operand = expression.getExpression();
            if (operand == null) {
                return;
            }
            checkForForLoopParam(operand);
            checkForForeachLoopParam(operand);
        }

        private void checkForForLoopParam(JSExpression expression) {
            if (!(expression instanceof JSReferenceExpression)) {
                return;
            }
            final JSReferenceExpression ref = (JSReferenceExpression) expression;
            final PsiElement element = ref.resolve();
            if (!(element instanceof JSVariable)) {
                return;
            }
            final JSVariable variable = (JSVariable) element;
            if(!(variable.getParent() instanceof JSVarStatement))
            {
                return;
            }
            final JSVarStatement decl = (JSVarStatement) variable.getParent();
            if (decl == null) {
                return;
            }
            if (!(decl.getParent() instanceof JSForStatement)) {
                return;
            }
            final JSForStatement forStatement = (JSForStatement) decl.getParent();
            assert forStatement != null;
            if (!isInForStatementBody(expression, forStatement)) {
                return;
            }
            registerError(expression);
        }

        private void checkForForeachLoopParam(JSExpression expression) {
            if (!(expression instanceof JSReferenceExpression)) {
                return;
            }
            final JSReferenceExpression ref = (JSReferenceExpression) expression;
            final PsiElement element = ref.resolve();
            if (!(element instanceof JSVariable)) {
                return;
            }
            final JSVariable parameter = (JSVariable) element;
            final PsiElement JSVarStatement = parameter.getParent();
            if (!(JSVarStatement.getParent() instanceof JSForInStatement)) {
                return;
            }
            registerError(expression);
        }

        private static boolean isInForStatementBody(JSExpression expression, JSForStatement statement) {
            final JSStatement body = statement.getBody();
            return PsiTreeUtil.isAncestor(body, expression, true);
        }
    }
}
