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

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 10:06:09 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSCatchBlockImpl extends JSElementImpl implements JSCatchBlock
{
	public JSCatchBlockImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSParameter getParameter()
	{
		final ASTNode node = getNode().findChildByType(JSElementTypes.FORMAL_PARAMETER);
		return node != null ? (JSParameter) node.getPsi() : null;
	}

	@Override
	public JSStatement getStatement()
	{
		final ASTNode node = getNode().findChildByType(JSElementTypes.STATEMENTS);
		return node != null ? (JSStatement) node.getPsi() : null;
	}

	@Override
	public void accept(@Nonnull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSCatchBlock(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent,
			@Nonnull PsiElement place)
	{
		if(lastParent != null)
		{
			final JSParameter param = getParameter();
			if(param != null)
			{
				return processor.execute(param, state);
			}
		}

		return true;
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		final ASTNode astNode = getNode();
		astNode.getTreeParent().removeChild(astNode);
	}
}
