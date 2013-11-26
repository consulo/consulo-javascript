package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import org.jetbrains.annotations.NotNull;

class CatchParameterUsedVisitor extends JSRecursiveElementVisitor {
    private final JSParameter parameter;
    private boolean used = false;

    CatchParameterUsedVisitor(JSParameter variable) {
        super();
        parameter = variable;
    }

    @Override public void visitElement(@NotNull PsiElement element) {
        if (!used) {
            super.visitElement(element);
        }
    }

    @Override public void visitJSReferenceExpression(@NotNull JSReferenceExpression reference) {
        if (used) {
            return;
        }
        super.visitJSReferenceExpression(reference);
        final PsiElement element = reference.resolve();
        if (parameter.equals(element)) {
            used = true;
        }
    }

    public boolean isUsed() {
        return used;
    }
}
