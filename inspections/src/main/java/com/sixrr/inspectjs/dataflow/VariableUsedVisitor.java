package com.sixrr.inspectjs.dataflow;

import javax.annotation.Nonnull;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;

public class VariableUsedVisitor extends JSRecursiveElementVisitor {

    private boolean used = false;
    @Nonnull
	private final JSVariable variable;

    public VariableUsedVisitor(@Nonnull JSVariable variable) {
        super();
        this.variable = variable;
    }

    @Override public void visitElement(@Nonnull PsiElement element) {
        if (!used) {
            super.visitElement(element);
        }
    }

    @Override public void visitJSReferenceExpression(@Nonnull JSReferenceExpression ref) {
        if (used) {
            return;
        }
        super.visitJSReferenceExpression(ref);
        PsiElement element = ref.resolve();
        final JSElement referent = element instanceof JSElement ? (JSElement) element:null;
        if (referent == null) {
            return;
        }
        if (referent.equals(variable)) {
            used = true;
        }
    }

    public boolean isUsed() {
        return used;
    }
}