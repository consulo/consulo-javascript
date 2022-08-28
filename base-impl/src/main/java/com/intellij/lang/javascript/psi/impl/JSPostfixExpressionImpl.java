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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSPostfixExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;

import javax.annotation.Nonnull;

/**
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:52:13 PM
 */
public class JSPostfixExpressionImpl extends JSExpressionImpl implements JSPostfixExpression
{
	public JSPostfixExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public JavaScriptType getType()
	{
		JSExpression expression = getExpression();
		return expression == null ? JavaScriptType.UNKNOWN : expression.getType();
	}

	@Override
	public JSExpression getExpression()
	{
		return findChildByClass(JSExpression.class);
	}

	@Override
	public IElementType getOperationSign()
	{
		final ASTNode[] nodes = getNode().getChildren(JSTokenTypes.OPERATIONS);
		return nodes.length == 1 ? nodes[0].getElementType() : null;
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSPostfixExpression(this);
	}
}
