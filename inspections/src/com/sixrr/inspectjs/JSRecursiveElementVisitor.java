package com.sixrr.inspectjs;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.psi.PsiElement;

public class JSRecursiveElementVisitor extends JSElementVisitor {
    @Override public void visitElement(PsiElement element) {

        super.visitElement(element);
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            child.accept(this);
        }
    }
}
