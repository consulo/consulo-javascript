package com.sixrr.inspectjs.dataflow;

import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import consulo.language.psi.PsiElement;

public class VariableUsedInInnerFunctionVisitor extends JSRecursiveElementVisitor {
    private final JSVariable variable;
    private boolean usedInInnerFunction = false;
    private boolean inInnerFunction = false;

    VariableUsedInInnerFunctionVisitor(JSVariable variable) {
        super();
        this.variable = variable;
    }

    @Override
    public void visitElement(PsiElement element) {
        if (usedInInnerFunction) {
            return;
        }
        super.visitElement(element);
    }

    @Override
    public void visitJSFunctionExpression(JSFunctionExpression funcExpr) {
        if (usedInInnerFunction) {
            return;
        }
        boolean wasInInnerFunction = inInnerFunction;
        inInnerFunction = true;
        super.visitJSFunctionExpression(funcExpr);
        inInnerFunction = wasInInnerFunction;
    }

    @Override
    public void visitJSReferenceExpression(JSReferenceExpression reference) {
        if (usedInInnerFunction) {
            return;
        }
        super.visitJSReferenceExpression(reference);
        if (inInnerFunction) {
            PsiElement element = reference.resolve();
            if (variable.equals(element)) {
                usedInInnerFunction = true;
            }
        }
    }

    public boolean isUsedInInnerFunction() {
        return usedInInnerFunction;
    }
}
