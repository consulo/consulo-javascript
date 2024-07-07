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

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.ast.TokenSet;
import consulo.annotation.access.RequiredReadAction;

import jakarta.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:20:30 PM
 */
public class JSForInStatementImpl extends JSStatementImpl implements JSForInStatement
{
	private static final TokenSet ourSeparatorElementType = TokenSet.create(JSTokenTypes.IN_KEYWORD, JSTokenTypes.OF_KEYWORD);

	public JSForInStatementImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSVarStatement getDeclarationStatement()
	{
		return findChildByClass(JSVarStatement.class);
	}

	@Override
	public JSExpression getVariableExpression()
	{
		ASTNode child = getNode().getFirstChildNode();
		while(child != null)
		{
			if(ourSeparatorElementType.contains(child.getElementType()))
			{
				return null;
			}
			if(child.getPsi() instanceof JSExpression)
			{
				return (JSExpression) child.getPsi();
			}
			child = child.getTreeNext();
		}
		return null;
	}

	@Override
	public JSExpression getCollectionExpression()
	{
		ASTNode child = getNode().getFirstChildNode();
		boolean inPassed = false;
		while(child != null)
		{
			if(ourSeparatorElementType.contains(child.getElementType()))
			{
				inPassed = true;
			}
			if(inPassed && child.getPsi() instanceof JSExpression)
			{
				return (JSExpression) child.getPsi();
			}
			child = child.getTreeNext();
		}

		return null;
	}

	@Override
	@RequiredReadAction
	public boolean isForEach()
	{
		return findChildByType(TokenSet.create(JSTokenTypes.EACH_KEYWORD, JSTokenTypes.OF_KEYWORD)) != null ;
	}

	@Override
	public JSStatement getBody()
	{
		ASTNode child = getNode().getFirstChildNode();
		boolean passedRParen = false;
		while(child != null)
		{
			if(child.getElementType() == JSTokenTypes.RPAR)
			{
				passedRParen = true;
			}
			else if(passedRParen && child.getPsi() instanceof JSStatement)
			{
				return (JSStatement) child.getPsi();
			}
			child = child.getTreeNext();
		}

		return null;
	}

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent, @Nonnull PsiElement place)
	{
		if(lastParent != null)
		{
			final JSVarStatement statement = getDeclarationStatement();
			if(statement != null)
			{
				return statement.processDeclarations(processor, state, lastParent, place);
			}
			else
			{
				final JSExpression expression = getVariableExpression();
				if(expression != null && !processor.execute(expression, null))
				{
					return false;
				}
			}
		}
		return true;
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSForInStatement(this);
	}
}
