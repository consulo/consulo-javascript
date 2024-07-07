package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import jakarta.annotation.Nonnull;

class LoopCountVisitor extends JSRecursiveElementVisitor {
    private int loopCount = 0;

    @Override public void visitJSElement(JSElement jsElement) {
        int oldCount = 0;
        if (jsElement instanceof JSFunction) {
            oldCount = loopCount;
        }
        super.visitJSElement(jsElement);

        if (jsElement instanceof JSFunction) {
            loopCount = oldCount;
        }
    }

    @Override public void visitJSForStatement(@Nonnull JSForStatement jsForStatement) {
        super.visitJSForStatement(jsForStatement);
        loopCount++;
    }

    @Override public void visitJSForInStatement(@Nonnull JSForInStatement jsForStatement) {
        super.visitJSForInStatement(jsForStatement);
        loopCount++;
    }

    @Override public void visitJSWhileStatement(@Nonnull JSWhileStatement jsWhileStatement) {
        super.visitJSWhileStatement(jsWhileStatement);
        loopCount++;
    }

    @Override public void visitJSDoWhileStatement(@Nonnull JSDoWhileStatement jsDoWhileStatement) {
        super.visitJSDoWhileStatement(jsDoWhileStatement);
        loopCount++;
    }

    public int getCount() {
        return loopCount;
    }
}
