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

import javax.annotation.Nonnull;

import com.intellij.icons.AllIcons;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.tree.TokenSet;
import consulo.ui.image.Image;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:36:30 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSObjectLiteralExpressionImpl extends JSExpressionImpl implements JSObjectLiteralExpression
{
	private static final TokenSet PROPERTIES_FILTER = TokenSet.create(JSElementTypes.PROPERTY);

	public JSObjectLiteralExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSProperty[] getProperties()
	{
		final ASTNode[] nodes = getNode().getChildren(PROPERTIES_FILTER);
		final JSProperty[] properties = new JSProperty[nodes.length];
		for(int i = 0; i < properties.length; i++)
		{
			properties[i] = (JSProperty) nodes[i].getPsi();
		}
		return properties;
	}

	@Override
	public void accept(@Nonnull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSObjectLiteralExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	public Image getIcon(int flags)
	{
		return AllIcons.Nodes.Class;
	}

	@Override
	public boolean processDeclarations(@Nonnull final PsiScopeProcessor processor, @Nonnull final ResolveState state, final PsiElement lastParent,
			@Nonnull final PsiElement place)
	{
		if(lastParent == null || !(place instanceof JSProperty))
		{
			return true;
		}
		final JSProperty[] props = getProperties();

		for(JSProperty property : props)
		{
			if(!processor.execute(property, state))
			{
				return false;
			}
		}

		return true;
	}
}
