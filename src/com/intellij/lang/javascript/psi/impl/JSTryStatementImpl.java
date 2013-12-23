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
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:59:44 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSTryStatementImpl extends JSStatementImpl implements JSTryStatement
{
	private static TokenSet ourCatchesTypeSet = TokenSet.create(JSElementTypes.CATCH_BLOCK);

	public JSTryStatementImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSStatement getStatement()
	{
		ASTNode child = getNode().getFirstChildNode();
		while(child != null)
		{
			final IElementType type = child.getElementType();
			if(JSElementTypes.STATEMENTS.contains(type))
			{
				return (JSStatement) child.getPsi();
			}
			if(type == JSTokenTypes.FINALLY_KEYWORD)
			{
				break;
			}
			child = child.getTreeNext();
		}
		return null;
	}

	@Override
	@Nullable
	public JSCatchBlock getCatchBlock()
	{
		final ASTNode catchChild = getNode().findChildByType(JSElementTypes.CATCH_BLOCK);
		if(catchChild == null)
		{
			return null;
		}
		return (JSCatchBlock) catchChild.getPsi();
	}

	@Override
	public JSCatchBlock[] getAllCatchBlocks()
	{
		return findChildrenByClass(JSCatchBlock.class);
	}

	@Override
	public JSStatement getFinallyStatement()
	{
		ASTNode child = getNode().getFirstChildNode();
		boolean foundFinally = false;
		while(child != null)
		{
			final IElementType type = child.getElementType();
			if(foundFinally && JSElementTypes.STATEMENTS.contains(type))
			{
				return (JSStatement) child.getPsi();
			}
			if(type == JSTokenTypes.FINALLY_KEYWORD)
			{
				foundFinally = true;
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
			((JSElementVisitor) visitor).visitJSTryStatement(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}
}
