package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import org.jetbrains.annotations.NotNull;

class CyclomaticComplexityVisitor extends JSRecursiveElementVisitor {
    private int complexity = 1;

    @Override public void visitJSElement(JSElement jsElement) {
        int oldComplexity = 0;
        if(jsElement instanceof JSFunction)
        {
            oldComplexity = complexity;
        }
        super.visitJSElement(jsElement);

        if (jsElement instanceof JSFunction) {
            complexity = oldComplexity;
        }
    }

    @Override public void visitJSForStatement(@NotNull JSForStatement statement) {
        super.visitJSForStatement(statement);
        complexity++;
    }

    public void visitJSForInStatement(@NotNull JSForInStatement statement) {
        super.visitJSForInStatement(statement);
        complexity++;
    }

    @Override public void visitJSIfStatement(@NotNull JSIfStatement statement) {
        super.visitJSIfStatement(statement);
        complexity++;
    }

    @Override public void visitJSDoWhileStatement(@NotNull JSDoWhileStatement statement) {
        super.visitJSDoWhileStatement(statement);
        complexity++;
    }

    @Override public void visitJSConditionalExpression(JSConditionalExpression expression) {
        super.visitJSConditionalExpression(expression);
        complexity++;
    }

    @Override public void visitJSSwitchStatement(@NotNull JSSwitchStatement statement) {
        super.visitJSSwitchStatement(statement);
        final JSCaseClause[] caseClauses = statement.getCaseClauses();
        for (JSCaseClause clause : caseClauses) {
            final JSStatement[] statements = clause.getStatements();
            if(statements!=null && statements.length!=0)
            {
                complexity++;
            }
        }
    }

    @Override public void visitJSWhileStatement(@NotNull JSWhileStatement statement) {
        super.visitJSWhileStatement(statement);
        complexity++;
    }

    public int getComplexity() {
        return complexity;
    }
}
