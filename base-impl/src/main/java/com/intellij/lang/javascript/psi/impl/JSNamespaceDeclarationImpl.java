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
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSNamespaceDeclaration;
import com.intellij.lang.javascript.psi.stubs.JSNamespaceDeclarationStub;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import consulo.javascript.lang.JavaScriptTokenSets;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

/**
 * @by Maxim.Mossienko
 */
public class JSNamespaceDeclarationImpl extends JSStubbedStatementImpl<JSNamespaceDeclarationStub> implements JSNamespaceDeclaration
{
	public JSNamespaceDeclarationImpl(final ASTNode node)
	{
		super(node);
	}

	public JSNamespaceDeclarationImpl(final JSNamespaceDeclarationStub node)
	{
		super(node, JSElementTypes.NAMESPACE_DECLARATION);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSNamespaceDeclaration(this);
	}

	@Override
	public JSAttributeList getAttributeList()
	{
		return getStubOrPsiChild(JSElementTypes.ATTRIBUTE_LIST);
	}

	@Override
	public PsiElement setName(@NonNls @Nonnull String newName) throws IncorrectOperationException
	{
		final String oldName = getName();
		if(newName.equals(oldName))
		{
			return this;
		}

		getNode().replaceChild(getNameIdentifier().getNode(), JSChangeUtil.createExpressionFromText(getProject(), newName).getNode());

		JSPsiImplUtils.updateFileName(this, newName, oldName);
		return this;
	}

	@Override
	public String getName()
	{
		final JSNamespaceDeclarationStub stub = getStub();
		if(stub != null)
		{
			return stub.getName();
		}
		final PsiElement node = getNameIdentifier();
		return node != null ? node.getText() : null;
	}

	@Override
	public int getTextOffset()
	{
		final PsiElement node = getNameIdentifier();
		return node == null ? super.getTextOffset() : node.getTextOffset();
	}

	@Override
	public String getQualifiedName()
	{
		final JSNamespaceDeclarationStub stub = getStub();
		if(stub != null)
		{
			return stub.getQualifiedName();
		}
		return JSPsiImplUtils.getQName(this);
	}

	@Override
	public PsiElement getNameIdentifier()
	{
		return findChildByType(JSElementTypes.REFERENCE_EXPRESSION);
	}

	@Override
	public String getInitialValueString()
	{
		final JSNamespaceDeclarationStub stub = getStub();
		if(stub != null)
		{
			return stub.getInitialValueString();
		}
		final ASTNode anchor = getNode().findChildByType(JSTokenTypes.EQ);

		if(anchor != null)
		{
			ASTNode node = anchor.getTreeNext();

			if(node != null && node.getElementType() == JSTokenTypes.WHITE_SPACE)
			{
				node = node.getTreeNext();
			}

			if(node != null && JavaScriptTokenSets.STRING_LITERALS.contains(node.getElementType()))
			{
				return node.getText();
			}
		}
		return null;
	}

	@Override
	public boolean isDeprecated()
	{
		return false;
	}

	@Override
	public boolean processDeclarations(@Nonnull final PsiScopeProcessor processor, @Nonnull final ResolveState state, final PsiElement lastParent,
									   @Nonnull final PsiElement place)
	{
		return processor.execute(lastParent, state);
	}
}
