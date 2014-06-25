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

package com.intellij.lang.javascript.psi.impl;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.tree.IElementType;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:47:35 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSConditionalExpressionImpl extends JSExpressionImpl implements JSConditionalExpression
{
	public JSConditionalExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSExpression getCondition()
	{
		ASTNode child = getNode().getFirstChildNode();
		while(child != null)
		{
			final IElementType type = child.getElementType();
			if(type == JSTokenTypes.QUEST)
			{
				return null;
			}
			if(JSElementTypes.EXPRESSIONS.contains(type))
			{
				return (JSExpression) child.getPsi();
			}
			child = child.getTreeNext();
		}
		return null;
	}

	@Override
	public JSExpression getThen()
	{
		boolean questPassed = false;
		ASTNode child = getNode().getFirstChildNode();
		while(child != null)
		{
			final IElementType type = child.getElementType();
			if(type == JSTokenTypes.QUEST)
			{
				questPassed = true;
			}
			if(type == JSTokenTypes.COLON)
			{
				return null;
			}
			if(questPassed && JSElementTypes.EXPRESSIONS.contains(type))
			{
				return (JSExpression) child.getPsi();
			}

			child = child.getTreeNext();
		}
		return null;
	}

	@Override
	public JSExpression getElse()
	{
		boolean questPassed = false;
		boolean colonPassed = false;
		ASTNode child = getNode().getFirstChildNode();
		while(child != null)
		{
			final IElementType type = child.getElementType();
			if(type == JSTokenTypes.QUEST)
			{
				questPassed = true;
			}
			if(type == JSTokenTypes.COLON)
			{
				colonPassed = true;
			}
			if(questPassed && colonPassed && JSElementTypes.EXPRESSIONS.contains(type))
			{
				return (JSExpression) child.getPsi();
			}

			child = child.getTreeNext();
		}
		return null;
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSConditionalExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}
}
