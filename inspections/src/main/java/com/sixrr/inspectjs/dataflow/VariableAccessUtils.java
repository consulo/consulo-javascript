package com.sixrr.inspectjs.dataflow;

import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;

public class VariableAccessUtils {
    public static boolean mayEvaluateToVariable(JSExpression expression, JSVariable variable) {
        if (expression == null) {
            return false;
        }
        if (expression instanceof JSParenthesizedExpression parenthesized) {
            JSExpression containedExpression = parenthesized.getInnerExpression();
            return mayEvaluateToVariable(containedExpression, variable);
        }
        if (expression instanceof JSDefinitionExpression definition) {
            JSExpression containedExpression = definition.getExpression();
            return mayEvaluateToVariable(containedExpression, variable);
        }
        if (expression instanceof JSConditionalExpression conditional) {
            JSExpression thenExpression = conditional.getThen();
            JSExpression elseExpression = conditional.getElse();
            return mayEvaluateToVariable(thenExpression, variable) || mayEvaluateToVariable(elseExpression, variable);
        }
        if (!(expression instanceof JSReferenceExpression)) {
            return false;
        }
        PsiElement referent = ((PsiReference)expression).resolve();
        if (referent == null) {
            return false;
        }
        return referent.equals(variable);
    }

    public static boolean variableIsUsed(JSVariable variable, PsiElement context) {
        VariableUsedVisitor visitor = new VariableUsedVisitor(variable);
        context.accept(visitor);
        return visitor.isUsed();
    }
}
