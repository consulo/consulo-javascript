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
import com.intellij.lang.javascript.psi.JSLetExpression;

import javax.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: maxim.mossienko
 * Date: Dec 14, 2005
 * Time: 6:40:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSLetExpressionImpl extends JSExpressionImpl implements JSLetExpression
{
	public JSLetExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSExpression getExpression()
	{
		return findChildByClass(JSExpression.class);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSLetExpression(this);
	}
}