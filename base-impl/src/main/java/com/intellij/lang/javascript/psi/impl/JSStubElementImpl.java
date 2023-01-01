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

import com.intellij.lang.javascript.index.JSItemPresentation;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSNamedElement;
import consulo.content.scope.SearchScope;
import consulo.language.ast.ASTNode;
import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.StubElement;
import consulo.language.util.IncorrectOperationException;
import consulo.navigation.ItemPresentation;
import consulo.navigation.NavigationItem;
import consulo.util.dataholder.Key;

import javax.annotation.Nonnull;

/**
 * User: max
 * Date: Jan 30, 2005
 * Time: 8:23:10 PM
 */
public abstract class JSStubElementImpl<T extends StubElement> extends StubBasedPsiElementBase<T> implements JSElement
{
	public static Key<NavigationItem> ORIGINAL_ELEMENT = Key.create("ORIGINAL_NAMED_ELEMENT");

	public JSStubElementImpl(final ASTNode node)
	{
		super(node);
	}

	public JSStubElementImpl(final T t, IStubElementType type)
	{
		super(t, type);
	}

	protected abstract void accept(@Nonnull JSElementVisitor visitor);

	@Override
	public final void accept(@Nonnull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			accept((JSElementVisitor)visitor);
		}
		else
		{
			super.accept(visitor);
		}
	}

	public SearchScope getDefaultUseScope()
	{
		return super.getUseScope();
	}

	@Override
	public ItemPresentation getPresentation()
	{
		if(this instanceof JSNamedElement)
		{
			final NavigationItem element = getUserData(ORIGINAL_ELEMENT);
			if(element == null)
			{
				return new JSItemPresentation((JSNamedElement) this);
			}
			return element.getPresentation();
		}
		return null;
	}

	@Override
	public PsiElement addBefore(@Nonnull PsiElement element, PsiElement anchor) throws IncorrectOperationException
	{
		if(JSChangeUtil.isStatementOrComment(element))
		{
			if(JSChangeUtil.isStatementContainer(this))
			{
				return JSChangeUtil.doAddBefore(this, element, anchor);
			}
			else if(JSChangeUtil.isBlockStatementContainer(this) && anchor != null)
			{
				return JSChangeUtil.blockDoAddBefore(element, anchor);
			}
		}

		return super.addBefore(element, anchor);
	}

	@Override
	public PsiElement addAfter(@Nonnull PsiElement element, PsiElement anchor) throws IncorrectOperationException
	{
		if(JSChangeUtil.isStatementOrComment(element))
		{
			if(JSChangeUtil.isStatementContainer(this))
			{
				return JSChangeUtil.doAddAfter(this, element, anchor);
			}
			else if(JSChangeUtil.isBlockStatementContainer(this) && anchor != null)
			{
				return JSChangeUtil.blockDoAddAfter(element, anchor);
			}
		}
		return super.addAfter(element, anchor);
	}

	@Override
	public PsiElement addRangeBefore(@Nonnull PsiElement first, @Nonnull PsiElement last, PsiElement anchor) throws IncorrectOperationException
	{
		if(JSChangeUtil.isStatementOrComment(first))
		{
			if(JSChangeUtil.isStatementContainer(this))
			{
				return JSChangeUtil.doAddRangeBefore(this, first, last, anchor);
			}
			else if(JSChangeUtil.isBlockStatementContainer(this) && anchor != null)
			{
				return JSChangeUtil.blockDoAddRangeBefore(first, last, anchor);
			}
		}
		return super.addRangeBefore(first, last, anchor);
	}

	@Override
	public PsiElement addRangeAfter(PsiElement first, PsiElement last, PsiElement anchor) throws IncorrectOperationException
	{
		if(JSChangeUtil.isStatementOrComment(first))
		{
			if(JSChangeUtil.isStatementContainer(this))
			{
				return JSChangeUtil.doAddRangeAfter(this, first, last, anchor);
			}
			else if(JSChangeUtil.isBlockStatementContainer(this) && anchor != null)
			{
				return JSChangeUtil.blockDoAddRangeAfter(first, last, anchor);
			}
		}

		return super.addRangeAfter(first, last, anchor);
	}

	@Override
	public PsiElement add(@Nonnull PsiElement element) throws IncorrectOperationException
	{
		return addAfter(element, null);
	}

	@Override
	public PsiElement addRange(PsiElement first, PsiElement last) throws IncorrectOperationException
	{
		return addRangeAfter(first, last, null);
	}

	@Override
	public PsiElement replace(@Nonnull PsiElement newElement) throws IncorrectOperationException
	{
		final ASTNode myNode = getNode();
		final ASTNode result = newElement.getNode().copyElement();
		myNode.getTreeParent().replaceChild(myNode, result);
		return result.getPsi();
	}

	@Override
	public PsiElement getParent()
	{
		return getParentByStub();
	}
}