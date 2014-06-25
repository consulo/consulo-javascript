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
import com.intellij.ide.IconDescriptorUpdaters;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;

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

	@Override
	public JSExpression getExpression()
	{
		final ASTNode expressionNode = getNode().findChildByType(JSElementTypes.EXPRESSIONS);
		return expressionNode != null ? (JSExpression) expressionNode.getPsi() : null;
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSExpressionStatement(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@Override
	public boolean processDeclarations(@NotNull PsiScopeProcessor processor, @NotNull ResolveState state, PsiElement lastParent,
			@NotNull PsiElement place)
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

			public TextAttributesKey getTextAttributesKey()
			{
				return null;
			}

			@Override
			public Icon getIcon(boolean open)
			{
				return IconDescriptorUpdaters.getIcon(JSExpressionStatementImpl.this, 0);
			}
		};

	}
}
