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

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.JSSuppressionHolder;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.IncorrectOperationException;

/**
 * @by Maxim.Mossienko
 */
public class JSClassImpl extends JSClassBase implements JSSuppressionHolder
{
	private static final TokenSet ourNameTokenTypes = TokenSet.create(JSElementTypes.REFERENCE_EXPRESSION, JSTokenTypes.IDENTIFIER);

	public JSClassImpl(final ASTNode node)
	{
		super(node);
	}

	public JSClassImpl(final JSClassStub stub, JSStubElementType<JSClassStub, JSClass> elementType)
	{
		super(stub, elementType);
	}

	@RequiredReadAction
	@Override
	public int getTextOffset()
	{
		PsiElement nameIdentifier = getNameIdentifier();
		return nameIdentifier == null ? super.getTextOffset() : nameIdentifier.getTextOffset();
	}

	@Override
	public JSAttributeList getAttributeList()
	{
		return getStubOrPsiChild(JSElementTypes.ATTRIBUTE_LIST);
	}

	@Override
	@RequiredReadAction
	public String getName()
	{
		final JSClassStub classStub = getStub();
		if(classStub != null)
		{
			return classStub.getName();
		}

		PsiElement nameIdentifier = getNameIdentifier();
		if(nameIdentifier instanceof JSReferenceExpression)
		{
			return ((JSReferenceExpression) nameIdentifier).getReferencedName();
		}
		else if(nameIdentifier != null)
		{
			return nameIdentifier.getText();
		}
		return null;
	}

	@Override
	@RequiredReadAction
	public PsiElement setName(@NonNls @NotNull String newName) throws IncorrectOperationException
	{
		newName = newName.substring(newName.lastIndexOf('.') + 1);
		final String oldName = getName();
		if(newName.equals(oldName))
		{
			return this;
		}
		final JSFunction constructor = findFunctionByName(oldName);

		PsiElement nameIdentifier = getNameIdentifier();
		assert nameIdentifier != null;
		getNode().replaceChild(nameIdentifier.getNode(), JSChangeUtil.createExpressionFromText(getProject(), newName).getNode());

		if(constructor != null)
		{
			constructor.setName(newName);
		}

		JSPsiImplUtils.updateFileName(this, newName, oldName);

		return this;
	}

	@Override
	@RequiredReadAction
	public PsiElement getNameIdentifier()
	{
		return findChildByType(ourNameTokenTypes);
	}

	@Override
	public JSReferenceList getExtendsList()
	{
		return getStubOrPsiChild(JSElementTypes.EXTENDS_LIST);
	}

	@Override
	public JSReferenceList getImplementsList()
	{
		return getStubOrPsiChild(JSElementTypes.IMPLEMENTS_LIST);
	}

	@Override
	public
	@NonNls
	String getQualifiedName()
	{
		final JSClassStub classStub = getStub();
		if(classStub != null)
		{
			return classStub.getQualifiedName();
		}
		return JSPsiImplUtils.getQName(this);
	}

	@Override
	public boolean isInterface()
	{
		final JSClassStub classStub = getStub();
		if(classStub != null)
		{
			return classStub.isInterface();
		}
		return getNode().findChildByType(JSTokenTypes.INTERFACE_KEYWORD) != null;
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		getNode().getTreeParent().removeChild(getNode());
	}

	@Override
	public boolean isDeprecated()
	{
		final JSClassStub stub = getStub();
		if(stub != null)
		{
			return stub.isDeprecated();
		}
		return JSDocumentationUtils.calculateDeprecated(this);
	}

	@Override
	protected boolean processMembers(final PsiScopeProcessor processor, final ResolveState substitutor, final PsiElement lastParent, final PsiElement place)
	{
		return JSResolveUtil.processDeclarationsInScope(this, processor, substitutor, lastParent, place);
	}

	@Override
	public boolean processDeclarations(@NotNull final PsiScopeProcessor processor, @NotNull final ResolveState substitutor, final PsiElement lastParent, @NotNull final PsiElement place)
	{
		boolean b = super.processDeclarations(processor, substitutor, lastParent, place);

		if(b && lastParent != null && lastParent.getParent() == this && getParent() instanceof JSFile)
		{
			b = JSImportHandlingUtil.tryResolveImports(processor, this, place);
		}
		return b;
	}

	@Override
	public PsiElement addAfter(@NotNull final PsiElement element, PsiElement anchor) throws IncorrectOperationException
	{
		if(anchor == null)
		{
			ASTNode node = getNode().findChildByType(JSTokenTypes.RBRACE);
			if(node != null)
			{
				PsiElement psiElement = super.addAfter(element, node.getTreePrev().getPsi());
				CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(getNode(), psiElement.getNode());
				return psiElement;
			}
		}

		final PsiElement psiElement = super.addAfter(element, anchor);
		CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(getNode(), psiElement.getNode());
		return psiElement;
	}

	@Override
	public PsiElement addBefore(@NotNull final PsiElement element, final PsiElement anchor) throws IncorrectOperationException
	{
		final PsiElement superElement = super.addBefore(element, anchor);
		CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(getNode(), superElement.getNode());
		return superElement;
	}

	@Override
	public boolean isEquivalentTo(PsiElement another)
	{
		return super.isEquivalentTo(another) ||
				(another instanceof JSFile &&
						((JSFile) another).getVirtualFile().getNameWithoutExtension().equals(getName()) &&
						another == getParent().getParent()) ||
				JSPsiImplUtils.isTheSameClass(another, this);
	}

	@Override
	public PsiElement getNavigationElement()
	{
		return JSPsiImplUtils.findTopLevelNavigatableElement(this);
	}

}
