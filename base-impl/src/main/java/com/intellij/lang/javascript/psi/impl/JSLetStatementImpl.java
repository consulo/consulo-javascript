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
import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLetStatement;

import javax.annotation.Nonnull;

/**
 * @author maxim
 */
public class JSLetStatementImpl extends JSStatementImpl implements JSLetStatement
{
	public JSLetStatementImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSLetStatement(this);
	}

	@Override
	public JSExpression[] getExpressions()
	{
		final ASTNode[] children = getNode().getChildren(JSElementTypes.EXPRESSIONS);
		if(children.length == 0)
		{
			return JSExpression.EMPTY_ARRAY;
		}
		JSExpression[] result = new JSExpression[children.length];
		for(int i = 0; i < children.length; i++)
		{
			result[i] = (JSExpression) children[i].getPsi();
		}
		return result;
	}

	@Override
	public JSBlockStatement getBody()
	{
		final ASTNode child = getNode().findChildByType(JSElementTypes.BLOCK_STATEMENT);
		if(child == null)
		{
			return null;
		}
		return (JSBlockStatement) child.getPsi();
	}
}