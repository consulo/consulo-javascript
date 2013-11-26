package com.intellij.lang.javascript.psi;

import com.intellij.psi.PsiFile;
import com.intellij.psi.StubBasedPsiElement;

/**
 * @author nik
 */
public interface JSFile extends PsiFile, JSElement {
  StubBasedPsiElement findStubbedElementAtOffset(final int offset, final Class<? extends StubBasedPsiElement> clazz);
  JSSourceElement[] getStatements();
  boolean isPredefined();
}
