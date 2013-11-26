package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.intellij.psi.tree.IElementType;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import org.jetbrains.annotations.NotNull;

class NegationCountVisitor extends JSRecursiveElementVisitor {
    private int negationCount = 0;

    @Override public void visitJSElement(JSElement jsElement) {
        int oldCount = 0;
        if (jsElement instanceof JSFunction) {
            oldCount = negationCount;
        }
        super.visitJSElement(jsElement);

        if (jsElement instanceof JSFunction) {
            negationCount = oldCount;
        }
    }

    @Override public void visitJSBinaryExpression(@NotNull JSBinaryExpression expression) {
        super.visitJSBinaryExpression(expression);
        final IElementType sign = expression.getOperationSign();
        if (JSTokenTypes.NE.equals(sign) || JSTokenTypes.NEQEQ.equals(sign)) {
            negationCount++;
        }
    }

    @Override public void visitJSPrefixExpression(@NotNull JSPrefixExpression expression) {
        super.visitJSPrefixExpression(expression);
        final IElementType sign = expression.getOperationSign();
        if (JSTokenTypes.EXCL.equals(sign)) {
            negationCount++;
        }
    }

    public int getNegationCount() {
        return negationCount;
    }
}
