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
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;

import javax.annotation.Nonnull;

/**
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:30:44 PM
 */
public class JSParenthesizedExpressionImpl extends JSExpressionImpl implements JSParenthesizedExpression
{
	public JSParenthesizedExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSExpression getInnerExpression()
	{
		return findChildByClass(JSExpression.class);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSParenthesizedExpression(this);
	}
}
