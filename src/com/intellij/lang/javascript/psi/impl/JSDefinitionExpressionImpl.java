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

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.icons.AllIcons;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
 * User: maxim.mossienko
 * Date: Dec 14, 2005
 * Time: 6:40:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSDefinitionExpressionImpl extends JSExpressionImpl implements JSDefinitionExpression
{
	public JSDefinitionExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSExpression getExpression()
	{
		final ASTNode expressionNode = getNode().findChildByType(JSElementTypes.EXPRESSIONS);
		return expressionNode != null ? (JSExpression) expressionNode.getPsi() : null;
	}

	@Override
	public String getName()
	{
		final JSExpression expression = getExpression();
		if(expression instanceof JSReferenceExpression)
		{
			return ((JSReferenceExpression) expression).getReferencedName();
		}
		return null;
	}

	@Override
	public PsiElement setName(@NotNull String name) throws IncorrectOperationException
	{
		final JSExpression expression = getExpression();
		if(expression instanceof JSReferenceExpressionImpl)
		{
			return ((JSReferenceExpressionImpl) expression).handleElementRenameInternal(name);
		}
		return null;
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSDefinitionExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	public Icon getIcon(int flags)
	{
		return AllIcons.Nodes.Variable;
	}

	@Override
	public boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent,
			@NotNull PsiElement place)
	{
		if(lastParent == null)
		{
			return processor.execute(this, state);
		}
		return true;
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		final PsiElement parent = getParent();

		if(parent instanceof JSAssignmentExpression)
		{
			final PsiElement grandParent = parent.getParent();

			if(grandParent instanceof JSStatement)
			{
				grandParent.delete();
				return;
			}
			else if(grandParent instanceof JSBinaryExpression)
			{
				((JSBinaryExpression) grandParent).getROperand().replace(((JSAssignmentExpression) parent).getROperand());
				return;
			}
			else if(grandParent instanceof JSVariable)
			{
				final JSExpression initializerExpression = ((JSVariable) grandParent).getInitializer();
				initializerExpression.replace(((JSAssignmentExpression) parent).getROperand());
				return;
			}
		}
		super.delete();
	}

	@Override
	@Nullable
	public ASTNode findNameIdentifier()
	{
		return null;
	}

	@Override
	public PsiElement getNameIdentifier()
	{
		final ASTNode node = findNameIdentifier();
		return node != null ? node.getPsi() : null;
	}
}
