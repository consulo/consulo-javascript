package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;

class NestingDepthVisitor extends JSRecursiveElementVisitor {
    private int m_maximumDepth = 0;
    private int m_currentDepth = 0;

    //public void visitJSBlockStatement(JSBlockStatement statement) {
    //    final PsiElement parent = statement.getParent();
    //    final boolean isAlreadyCounted =
    //            parent instanceof JSDoWhileStatement ||
    //            parent instanceof JSWhileStatement ||
    //            parent instanceof JSForStatement ||
    //            parent instanceof JSIfStatement ||
    //            parent instanceof JSForInStatement;
    //    if (!isAlreadyCounted) {
    //        enterScope();
    //    }
    //    super.visitJSBlock(statement);
    //    if (!isAlreadyCounted) {
    //        exitScope();
    //    }
    //}

    @Override
    public void visitJSDoWhileStatement(JSDoWhileStatement statement) {
        enterScope();
        super.visitJSDoWhileStatement(statement);
        exitScope();
    }

    @Override
    public void visitJSForStatement(JSForStatement statement) {
        enterScope();
        super.visitJSForStatement(statement);
        exitScope();
    }

    @Override
    public void visitJSIfStatement(JSIfStatement statement) {
        boolean isAlreadyCounted = false;
        if (statement.getParent() instanceof JSIfStatement) {
            JSIfStatement parent = (JSIfStatement)statement.getParent();
            assert parent != null;
            JSStatement elseBranch = parent.getElse();
            if (statement.equals(elseBranch)) {
                isAlreadyCounted = true;
            }
        }
        if (!isAlreadyCounted) {
            enterScope();
        }
        super.visitJSIfStatement(statement);
        if (!isAlreadyCounted) {
            exitScope();
        }
    }

    @Override
    public void visitJSTryStatement(JSTryStatement statement) {
        enterScope();
        super.visitJSTryStatement(statement);
        exitScope();
    }

    @Override
    public void visitJSSwitchStatement(JSSwitchStatement statement) {
        enterScope();
        super.visitJSSwitchStatement(statement);
        exitScope();
    }

    @Override
    public void visitJSWhileStatement(JSWhileStatement statement) {
        enterScope();
        super.visitJSWhileStatement(statement);
        exitScope();
    }

    private void enterScope() {
        m_currentDepth++;
        m_maximumDepth = Math.max(m_maximumDepth, m_currentDepth);
    }

    private void exitScope() {
        m_currentDepth--;
    }

    public int getMaximumDepth() {
        return m_maximumDepth;
    }
}
