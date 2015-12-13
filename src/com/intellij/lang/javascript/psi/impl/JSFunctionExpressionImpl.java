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
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.javascript.lang.psi.JavaScriptType;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:55:33 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSFunctionExpressionImpl extends JSFunctionBaseImpl<JSFunctionStub, JSFunctionExpression> implements JSFunctionExpression
{
	public JSFunctionExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	public JSFunctionExpressionImpl(final JSFunctionStub stub, IStubElementType type)
	{
		super(stub, type);
	}

	@NotNull
	@Override
	public JSFunction getFunction()
	{
		return this;
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSFunctionExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@Override
	protected ASTNode createNameIdentifier(final String name)
	{
		return JSChangeUtil.createNameIdentifier(getProject(), name);
	}

	@Override
	public JSExpression replace(JSExpression newExpr)
	{
		return JSChangeUtil.replaceExpression(this, newExpr);
	}

	@RequiredReadAction
	@NotNull
	@Override
	public JavaScriptType getType()
	{
		return JavaScriptType.UNKNOWN;
	}

	@Override
	public ASTNode findNameIdentifier()
	{
		final ASTNode treeParent = getNode().getTreeParent();
		PsiElement psi = treeParent != null ? treeParent.getPsi() : null;
		if(psi instanceof JSCallExpression)
		{
			psi = psi.getParent();
		}
		if(psi instanceof JSAssignmentExpression)
		{
			final JSExpression jsExpression = ((JSAssignmentExpression) psi).getLOperand();
			final JSExpression lOperand = jsExpression instanceof JSDefinitionExpression ? ((JSDefinitionExpression) jsExpression).getExpression() : null;

			if(lOperand instanceof JSReferenceExpression)
			{
				return lOperand.getNode().findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET);
			}
		}
		else if(psi instanceof JSProperty)
		{
			return psi.getNode().findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET);
		}
		else
		{
			final ASTNode node = super.findNameIdentifier();

			if(node != null)
			{
				return node;
			}

			if(psi instanceof JSVariable)
			{
				return psi.getNode().findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET);
			}
		}
		return null;
	}

	@Override
	public JSAttributeList getAttributeList()
	{
		return null;
	}

	@Override
	public int getTextOffset()
	{
		final ASTNode name = findNameIdentifier();
		return name != null ? name.getStartOffset() : super.getTextOffset();
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		final PsiElement parent = getParent();
		if(parent instanceof JSAssignmentExpression)
		{
			((JSAssignmentExpression) parent).getLOperand().delete();
			return;
		}
		super.delete();
	}

	@Override
	public boolean isGetProperty()
	{
		return false;
	}

	@Override
	public boolean isSetProperty()
	{
		return false;
	}

	@Override
	public boolean isConstructor()
	{
		return false;
	}

	@Override
	public String getQualifiedName()
	{
		return getName();
	}
}
