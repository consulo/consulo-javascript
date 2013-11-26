package com.sixrr.inspectjs.dataflow;

import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import org.jetbrains.annotations.NotNull;

public class VariableUsedInInnerFunctionVisitor extends JSRecursiveElementVisitor
{
    private final JSVariable variable;
    private boolean usedInInnerFunction = false;
    private boolean inInnerFunction = false;

    VariableUsedInInnerFunctionVisitor(JSVariable variable)
    {
        super();
        this.variable = variable;
    }

    @Override public void visitElement(@NotNull PsiElement element)
    {
        if (usedInInnerFunction)
        {
            return;
        }
        super.visitElement(element);
    }

    @Override public void visitJSFunctionExpression(
            @NotNull JSFunctionExpression funcExpr)
    {
        if(usedInInnerFunction)
        {
            return;
        }
        final boolean wasInInnerFunction = inInnerFunction;
        inInnerFunction = true;
        super.visitJSFunctionExpression(funcExpr);
        inInnerFunction = wasInInnerFunction;
    }

    @Override public void visitJSReferenceExpression(
            @NotNull JSReferenceExpression reference)
    {
        if(usedInInnerFunction)
        {
            return;
        }
        super.visitJSReferenceExpression(reference);
        if(inInnerFunction)
        {
            final PsiElement element = reference.resolve();
            if(variable.equals(element))
            {
                usedInInnerFunction = true;
            }
        }
    }

    public boolean isUsedInInnerFunction()
    {
        return usedInInnerFunction;
    }
}
