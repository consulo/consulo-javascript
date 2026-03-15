package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;

class LoopCountVisitor extends JSRecursiveElementVisitor {
    private int loopCount = 0;

    @Override
    public void visitJSElement(JSElement jsElement) {
        int oldCount = 0;
        if (jsElement instanceof JSFunction) {
            oldCount = loopCount;
        }
        super.visitJSElement(jsElement);

        if (jsElement instanceof JSFunction) {
            loopCount = oldCount;
        }
    }

    @Override
    public void visitJSForStatement(JSForStatement jsForStatement) {
        super.visitJSForStatement(jsForStatement);
        loopCount++;
    }

    @Override
    public void visitJSForInStatement(JSForInStatement jsForStatement) {
        super.visitJSForInStatement(jsForStatement);
        loopCount++;
    }

    @Override
    public void visitJSWhileStatement(JSWhileStatement jsWhileStatement) {
        super.visitJSWhileStatement(jsWhileStatement);
        loopCount++;
    }

    @Override
    public void visitJSDoWhileStatement(JSDoWhileStatement jsDoWhileStatement) {
        super.visitJSDoWhileStatement(jsDoWhileStatement);
        loopCount++;
    }

    public int getCount() {
        return loopCount;
    }
}
