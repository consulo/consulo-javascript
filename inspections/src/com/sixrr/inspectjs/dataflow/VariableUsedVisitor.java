package com.sixrr.inspectjs.dataflow;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import org.jetbrains.annotations.NotNull;

public class VariableUsedVisitor extends JSRecursiveElementVisitor {

    private boolean used = false;
    @NotNull private final JSVariable variable;

    public VariableUsedVisitor(@NotNull JSVariable variable) {
        super();
        this.variable = variable;
    }

    @Override public void visitElement(@NotNull PsiElement element) {
        if (!used) {
            super.visitElement(element);
        }
    }

    @Override public void visitJSReferenceExpression(@NotNull JSReferenceExpression ref) {
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