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

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSSuppressionHolder;
import com.intellij.lang.javascript.types.JSFileElementType;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.IncorrectOperationException;

/**
 * @author ven
 */
class JSStubbedStatementImpl<T extends StubElement> extends JSStubElementImpl<T> implements JSStatement, JSSuppressionHolder
{
	JSStubbedStatementImpl(final ASTNode node)
	{
		super(node);
	}

	JSStubbedStatementImpl(final T t, IStubElementType type)
	{
		super(t, type);
	}

	@Override
	public JSStatement addStatementBefore(JSStatement toAdd) throws IncorrectOperationException
	{
		return addStatementImpl(toAdd, true);
	}

	@Override
	public JSStatement addStatementAfter(JSStatement toAdd) throws IncorrectOperationException
	{
		return addStatementImpl(toAdd, false);
	}

	//TODO: [lesya] the formatter stuff definitely needs more intelligence
	private JSStatement addStatementImpl(final JSStatement toAdd, final boolean before) throws IncorrectOperationException
	{
		final ASTNode treeParent = getNode().getTreeParent();

		if(treeParent.getElementType() != JSElementTypes.BLOCK_STATEMENT &&
				!(treeParent.getElementType() instanceof JSFileElementType) &&
				treeParent.getElementType() != JSElementTypes.CLASS &&
				treeParent.getElementType() != JSElementTypes.EMBEDDED_CONTENT)
		{
			if(before)
			{
				return (JSStatement) treeParent.getPsi().addBefore(toAdd, this);
			}
			else
			{
				return (JSStatement) treeParent.getPsi().addAfter(toAdd, this);
			}
		}
		else
		{
			final ASTNode copy = toAdd.getNode().copyElement();
			addChildAndReformat(treeParent, copy, before ? getNode() : getNode().getTreeNext());
			return (JSStatement) copy.getPsi();
		}
	}

	private void addChildAndReformat(final ASTNode block, final ASTNode addedElement, final ASTNode anchorBefore) throws IncorrectOperationException
	{
		block.addChild(addedElement, anchorBefore);
		CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(block, addedElement);
	}

	@Override
	public JSStatement replace(JSStatement newStatement)
	{
		return JSChangeUtil.replaceStatement(this, newStatement);
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		getNode().getTreeParent().removeChild(getNode());
	}
}