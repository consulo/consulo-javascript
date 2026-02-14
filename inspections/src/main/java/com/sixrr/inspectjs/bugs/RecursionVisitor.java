package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import jakarta.annotation.Nonnull;

public class RecursionVisitor extends JSRecursiveElementVisitor {

    private boolean recursive = false;
    private final JSFunction function;
    private String functionName;

    public RecursionVisitor(@Nonnull JSFunction function) {
        super();
        this.function = function;
        functionName = function.getName();
    }

    @Override
    public void visitElement(@Nonnull PsiElement element) {
        if (!recursive) {
            super.visitElement(element);
        }
    }

    @Override
    public void visitJSCallExpression(@Nonnull JSCallExpression call) {
        if (recursive) {
            return;
        }
        super.visitJSCallExpression(call);
        JSExpression methodExpression = call.getMethodExpression();

        // method expression could be e.g. a["1"]
        if (!(methodExpression instanceof JSReferenceExpression)) {
            return;
        }
        JSReferenceExpression functionExpression = (JSReferenceExpression)methodExpression;
        String calledFunctionName = functionExpression.getReferencedName();

        if (calledFunctionName == null) {
            return;
        }
        if (!calledFunctionName.equals(functionName)) {
            return;
        }
        PsiElement referent = functionExpression.resolve();
        if (!function.equals(referent)) {
            return;
        }
        JSExpression qualifier = functionExpression.getQualifier();
        if (qualifier == null || qualifier instanceof JSThisExpression) {
            recursive = true;
        }
    }

    public boolean isRecursive() {
        return recursive;
    }
}
