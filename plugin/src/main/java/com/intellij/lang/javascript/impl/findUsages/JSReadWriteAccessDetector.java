/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.impl.findUsages;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSPostfixExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import consulo.language.ast.IElementType;
import consulo.language.editor.highlight.ReadWriteAccessDetector;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;

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
