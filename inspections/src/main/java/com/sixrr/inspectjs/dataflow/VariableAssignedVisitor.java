package com.sixrr.inspectjs.dataflow;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.ast.IElementType;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;

public class VariableAssignedVisitor extends JSRecursiveElementVisitor {
    private boolean assigned = false;
    private final JSVariable variable;

    public VariableAssignedVisitor(JSVariable variable) {
        super();
        this.variable = variable;
    }

    @Override
    public void visitElement(PsiElement element) {
        if (!assigned) {
            super.visitElement(element);
        }
    }

    @Override
    public void visitJSAssignmentExpression(JSAssignmentExpression assignment) {
        if (assigned) {
            return;
        }
        super.visitJSAssignmentExpression(assignment);
        JSExpression arg = assignment.getLOperand();
        if (VariableAccessUtils.mayEvaluateToVariable(arg, variable)) {
            assigned = true;
        }
    }

    @Override
    public void visitJSPrefixExpression(JSPrefixExpression expression) {
        if (assigned) {
            return;
        }
        super.visitJSPrefixExpression(expression);
        IElementType operationSign = expression.getOperationSign();
        if (!JSTokenTypes.PLUSPLUS.equals(operationSign) &&
            !JSTokenTypes.MINUSMINUS.equals(operationSign)) {
            return;
        }
        JSExpression operand = expression.getExpression();
        if (VariableAccessUtils.mayEvaluateToVariable(operand, variable)) {
            assigned = true;
        }
    }

    @Override
    public void visitJSPostfixExpression(JSPostfixExpression postfixExpression) {
        if (assigned) {
            return;
        }
        super.visitJSPostfixExpression(postfixExpression);
        IElementType operationSign = postfixExpression.getOperationSign();
        if (!JSTokenTypes.PLUSPLUS.equals(operationSign) &&
            !JSTokenTypes.MINUSMINUS.equals(operationSign)) {
            return;
        }
        JSExpression operand = postfixExpression.getExpression();
        if (VariableAccessUtils.mayEvaluateToVariable(operand, variable)) {
            assigned = true;
        }
    }

    public boolean isAssigned() {
        return assigned;
    }
}
