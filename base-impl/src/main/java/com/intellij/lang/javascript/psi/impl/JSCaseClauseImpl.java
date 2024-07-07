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
import com.intellij.lang.javascript.psi.JSCaseClause;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.language.ast.ASTNode;
import jakarta.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 10:11:23 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSCaseClauseImpl extends JSElementImpl implements JSCaseClause
{
	public JSCaseClauseImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public boolean isDefault()
	{
		return getNode().findChildByType(JSTokenTypes.DEFAULT_KEYWORD) != null;
	}

	@Override
	public JSExpression getCaseExpression()
	{
		if(isDefault())
		{
			return null;
		}
		return findChildByClass(JSExpression.class);
	}

	@Override
	public JSStatement[] getStatements()
	{
		return findChildrenByClass(JSStatement.class);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSCaseClause(this);
	}
}
