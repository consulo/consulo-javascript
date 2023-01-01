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

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.icon.IconDescriptorUpdaters;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.navigation.ItemPresentation;
import consulo.ui.image.Image;

import javax.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:26:39 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSExpressionStatementImpl extends JSStatementImpl implements JSExpressionStatement
{
	public JSExpressionStatementImpl(final ASTNode node)
	{
		super(node);
	}

	@RequiredReadAction
	@Override
	public JSExpression getExpression()
	{
		return findChildByClass(JSExpression.class);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSExpressionStatement(this);
	}

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent,
									   @Nonnull PsiElement place)
	{
		if(lastParent == null)
		{
			final JSExpression expression = getExpression();
			if(expression != null)
			{
				return expression.processDeclarations(processor, state, lastParent, place);
			}
		}

		return super.processDeclarations(processor, state, lastParent, place);
	}

	@Override
	public ItemPresentation getPresentation()
	{
		return new ItemPresentation()
		{
			@Override
			public String getPresentableText()
			{
				return getText();
			}

			@Override
			public String getLocationString()
			{
				return "";
			}

			@Override
			public Image getIcon()
			{
				return IconDescriptorUpdaters.getIcon(JSExpressionStatementImpl.this, 0);
			}
		};

	}
}
