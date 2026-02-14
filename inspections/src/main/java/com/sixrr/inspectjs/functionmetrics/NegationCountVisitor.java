package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import consulo.language.ast.IElementType;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import jakarta.annotation.Nonnull;

class NegationCountVisitor extends JSRecursiveElementVisitor {
    private int negationCount = 0;

    @Override
    public void visitJSElement(JSElement jsElement) {
        int oldCount = 0;
        if (jsElement instanceof JSFunction) {
            oldCount = negationCount;
        }
        super.visitJSElement(jsElement);

        if (jsElement instanceof JSFunction) {
            negationCount = oldCount;
        }
    }

    @Override
    public void visitJSBinaryExpression(@Nonnull JSBinaryExpression expression) {
        super.visitJSBinaryExpression(expression);
        IElementType sign = expression.getOperationSign();
        if (JSTokenTypes.NE.equals(sign) || JSTokenTypes.NEQEQ.equals(sign)) {
            negationCount++;
        }
    }

    @Override
    public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
        super.visitJSPrefixExpression(expression);
        IElementType sign = expression.getOperationSign();
        if (JSTokenTypes.EXCL.equals(sign)) {
            negationCount++;
        }
    }

    public int getNegationCount() {
        return negationCount;
    }
}
