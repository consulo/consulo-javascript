package com.sixrr.inspectjs.dataflow;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import org.jetbrains.annotations.NotNull;

public class VariableAssignedVisitor extends JSRecursiveElementVisitor {

    private boolean assigned = false;
    @NotNull private final JSVariable variable;

    public VariableAssignedVisitor(@NotNull JSVariable variable) {
        super();
        this.variable = variable;
    }

    @Override public void visitElement(@NotNull PsiElement element) {
        if (!assigned) {
            super.visitElement(element);
        }
    }

    @Override public void visitJSAssignmentExpression(
            @NotNull JSAssignmentExpression assignment) {
        if (assigned) {
            return;
        }
        super.visitJSAssignmentExpression(assignment);
        final JSExpression arg = assignment.getLOperand();
        if (VariableAccessUtils.mayEvaluateToVariable(arg, variable)) {
            assigned = true;
        }
    }

    @Override public void visitJSPrefixExpression(
            @NotNull JSPrefixExpression expression) {
        if (assigned) {
            return;
        }
        super.visitJSPrefixExpression(expression);
        final IElementType operationSign = expression.getOperationSign();
        if (!JSTokenTypes.PLUSPLUS.equals(operationSign) &&
                !JSTokenTypes.MINUSMINUS.equals(operationSign)) {
            return;
        }
        final JSExpression operand = expression.getExpression();
        if (VariableAccessUtils.mayEvaluateToVariable(operand, variable)) {
            assigned = true;
        }
    }

    @Override public void visitJSPostfixExpression(
            @NotNull JSPostfixExpression postfixExpression) {
        if (assigned) {
            return;
        }
        super.visitJSPostfixExpression(postfixExpression);
        final IElementType operationSign = postfixExpression.getOperationSign();
        if (!JSTokenTypes.PLUSPLUS.equals(operationSign) &&
                !JSTokenTypes.MINUSMINUS.equals(operationSign)) {
            return;
        }
        final JSExpression operand = postfixExpression.getExpression();
        if (VariableAccessUtils.mayEvaluateToVariable(operand, variable)) {
            assigned = true;
        }
    }

    public boolean isAssigned() {
        return assigned;
    }
}
