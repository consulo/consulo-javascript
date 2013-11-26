package com.sixrr.inspectjs.dataflow;

import com.intellij.psi.*;
import com.intellij.lang.javascript.psi.*;

public class VariableAccessUtils {
    public static boolean mayEvaluateToVariable(JSExpression expression,
                                                JSVariable variable) {
        if (expression == null) {
            return false;
        }
        if (expression instanceof JSParenthesizedExpression) {
            final JSExpression containedExpression =
                    ((JSParenthesizedExpression) expression).getInnerExpression();
            return mayEvaluateToVariable(containedExpression, variable);
        }
        if (expression instanceof JSDefinitionExpression) {
            final JSExpression containedExpression =
                    ((JSDefinitionExpression) expression).getExpression();
            return mayEvaluateToVariable(containedExpression, variable);
        }
        if (expression instanceof JSConditionalExpression) {
            final JSConditionalExpression conditional =
                    (JSConditionalExpression) expression;
            final JSExpression thenExpression = conditional.getThen();
            final JSExpression elseExpression = conditional.getElse();
            return mayEvaluateToVariable(thenExpression, variable) ||
                    mayEvaluateToVariable(elseExpression, variable);
        }
        if (!(expression instanceof JSReferenceExpression)) {
            return false;
        }
        final PsiElement referent = ((PsiReference) expression).resolve();
        if (referent == null) {
            return false;
        }
        return referent.equals(variable);
    }

    public static boolean variableIsUsed(JSVariable variable,
                                         PsiElement context) {
        final VariableUsedVisitor visitor
                = new VariableUsedVisitor(variable);
        context.accept(visitor);
        return visitor.isUsed();
    }
}
