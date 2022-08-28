package com.sixrr.inspectjs;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiRecursiveVisitor;

public class JSRecursiveElementVisitor extends JSElementVisitor implements PsiRecursiveVisitor
{
	@Override
	public void visitElement(PsiElement element)
	{
		super.visitElement(element);

		element.acceptChildren(this);
	}
}
