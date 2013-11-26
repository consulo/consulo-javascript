package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import org.jetbrains.annotations.NotNull;

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

    @Override public void visitJSForStatement(@NotNull JSForStatement jsForStatement) {
        super.visitJSForStatement(jsForStatement);
        loopCount++;
    }

    @Override public void visitJSForInStatement(@NotNull JSForInStatement jsForStatement) {
        super.visitJSForInStatement(jsForStatement);
        loopCount++;
    }

    @Override public void visitJSWhileStatement(@NotNull JSWhileStatement jsWhileStatement) {
        super.visitJSWhileStatement(jsWhileStatement);
        loopCount++;
    }

    @Override public void visitJSDoWhileStatement(@NotNull JSDoWhileStatement jsDoWhileStatement) {
        super.visitJSDoWhileStatement(jsDoWhileStatement);
        loopCount++;
    }

    public int getCount() {
        return loopCount;
    }
}
