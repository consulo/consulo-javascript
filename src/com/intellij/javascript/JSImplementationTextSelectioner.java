/*
 * User: anna
 * Date: 01-Feb-2008
 */
package com.intellij.javascript;

import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.hint.ImplementationTextSelectioner;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;

public class JSImplementationTextSelectioner implements ImplementationTextSelectioner
{
	public int getTextStartOffset(@NotNull PsiElement element)
	{
		return element.getTextOffset();
	}

	public int getTextEndOffset(@NotNull PsiElement element)
	{
		if(element instanceof JSNamedElementProxy)
		{
			element = ((JSNamedElementProxy) element).getElement();
		}
		if(element instanceof JSDefinitionExpression)
		{
			element = PsiTreeUtil.getParentOfType(element, JSExpressionStatement.class);
		}
		return element.getTextRange().getEndOffset();
	}
}