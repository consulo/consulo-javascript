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
import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:15:02 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSArgumentListImpl extends JSElementImpl implements JSArgumentList
{
	public JSArgumentListImpl(final ASTNode node)
	{
		super(node);
	}

	public JSExpression[] getArguments()
	{
		final ASTNode[] nodes = getNode().getChildren(JSElementTypes.EXPRESSIONS);
		final JSExpression[] exprs = new JSExpression[nodes.length];
		for(int i = 0; i < exprs.length; i++)
		{
			exprs[i] = (JSExpression) nodes[i].getPsi();
		}
		return exprs;
	}

	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSArgumentList(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		PsiElement element = getParent();
		element.replace(((JSCallExpression) element).getMethodExpression());
	}
}
