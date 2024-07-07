package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import jakarta.annotation.Nonnull;

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

    @Override public void visitJSForStatement(@Nonnull JSForStatement statement) {
        super.visitJSForStatement(statement);
        complexity++;
    }

    @Override
	public void visitJSForInStatement(@Nonnull JSForInStatement statement) {
        super.visitJSForInStatement(statement);
        complexity++;
    }

    @Override public void visitJSIfStatement(@Nonnull JSIfStatement statement) {
        super.visitJSIfStatement(statement);
        complexity++;
    }

    @Override public void visitJSDoWhileStatement(@Nonnull JSDoWhileStatement statement) {
        super.visitJSDoWhileStatement(statement);
        complexity++;
    }

    @Override public void visitJSConditionalExpression(JSConditionalExpression expression) {
        super.visitJSConditionalExpression(expression);
        complexity++;
    }

    @Override public void visitJSSwitchStatement(@Nonnull JSSwitchStatement statement) {
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

    @Override public void visitJSWhileStatement(@Nonnull JSWhileStatement statement) {
        super.visitJSWhileStatement(statement);
        complexity++;
    }

    public int getComplexity() {
        return complexity;
    }
}
