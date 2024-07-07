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

import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.util.IncorrectOperationException;
import consulo.language.ast.ASTNode;

import jakarta.annotation.Nonnull;

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
		return findChildByClass(JSExpression.class);
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
	public PsiElement setName(@Nonnull String name) throws IncorrectOperationException
	{
		final JSExpression expression = getExpression();
		if(expression instanceof JSReferenceExpressionImpl)
		{
			return ((JSReferenceExpressionImpl) expression).handleElementRenameInternal(name);
		}
		return null;
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSDefinitionExpression(this);
	}

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent,
									   @Nonnull PsiElement place)
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
	public PsiElement getNameIdentifier()
	{
		return null;
	}
}
