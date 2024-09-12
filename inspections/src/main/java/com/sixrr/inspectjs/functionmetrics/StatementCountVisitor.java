package com.sixrr.inspectjs.functionmetrics;

import jakarta.annotation.Nonnull;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;

class StatementCountVisitor extends JSRecursiveElementVisitor {
    private int statementCount = 0;

    @Override
    public void visitJSElement(JSElement jsElement) {
        int oldCount = 0;
        if (jsElement instanceof JSFunction) {
            oldCount = statementCount;
        }
        super.visitJSElement(jsElement);

        if (jsElement instanceof JSFunction) {
            statementCount = oldCount;
        }
    }

    @Override
    public void visitJSStatement(@Nonnull JSStatement statement) {
        super.visitJSStatement(statement);
        if (statement instanceof JSEmptyStatement || statement instanceof JSBlockStatement) {
            return;
        }
        statementCount++;
    }

    public int getStatementCount() {
        return statementCount;
    }
}
