package com.intellij.lang.javascript.findUsages;

import com.intellij.codeInsight.highlighting.ReadWriteAccessDetector;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSPostfixExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.tree.IElementType;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: 15.04.2009
 * Time: 21:51:25
 * To change this template use File | Settings | File Templates.
 */
public class JSReadWriteAccessDetector extends ReadWriteAccessDetector
{
	@Override
	public boolean isReadWriteAccessible(PsiElement element)
	{
		return element instanceof JSVariable ||
				((element instanceof JSFunction) && (((JSFunction) element).isGetProperty() || ((JSFunction) element).isSetProperty())) ||
				element instanceof JSDefinitionExpression;
	}

	@Override
	public boolean isDeclarationWriteAccess(PsiElement element)
	{
		return (element instanceof JSVariable && ((JSVariable) element).getInitializer() != null);
	}

	@Override
	public Access getReferenceAccess(PsiElement referencedElement, PsiReference reference)
	{
		return getExpressionAccess(reference.getElement());
	}

	@Override
	public Access getExpressionAccess(PsiElement expression)
	{
		expression = expression.getParent();
		if(expression instanceof JSDefinitionExpression)
		{
			PsiElement grandParent = expression.getParent();
			if(grandParent instanceof JSAssignmentExpression && ((JSAssignmentExpression) grandParent).getOperationSign() == JSTokenTypes.EQ)
			{
				return Access.Write;
			}

			return Access.ReadWrite;
		}
		if(expression instanceof JSPrefixExpression)
		{
			if(isIncrementOrDecrement(((JSPrefixExpression) expression).getOperationSign()))
			{
				return Access.ReadWrite;
			}
		}
		else if(expression instanceof JSPostfixExpression)
		{
			if(isIncrementOrDecrement(((JSPostfixExpression) expression).getOperationSign()))
			{
				return Access.ReadWrite;
			}
		}
		return Access.Read;
	}

	private static boolean isIncrementOrDecrement(IElementType sign)
	{
		return sign == JSTokenTypes.PLUSPLUS || sign == JSTokenTypes.MINUSMINUS;
	}
}
