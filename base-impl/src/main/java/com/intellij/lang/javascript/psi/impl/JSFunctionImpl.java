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
import consulo.annotations.RequiredReadAction;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSSuppressionHolder;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.openapi.util.Comparing;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 8:25:27 PM
 */
public class JSFunctionImpl extends JSFunctionBaseImpl<JSFunctionStub, JSFunction> implements JSSuppressionHolder
{
	public JSFunctionImpl(final ASTNode node)
	{
		super(node);
	}

	public JSFunctionImpl(final JSFunctionStub stub, IStubElementType type)
	{
		super(stub, type);
	}

	@RequiredReadAction
	@Override
	public boolean isGetProperty()
	{
		final JSFunctionStub stub = getStub();
		if(stub != null)
		{
			return stub.isGetProperty();
		}
		return findChildByType(JSTokenTypes.GET_KEYWORD) != null;
	}

	@RequiredReadAction
	@Override
	public boolean isSetProperty()
	{
		final JSFunctionStub stub = getStub();
		if(stub != null)
		{
			return stub.isGetProperty();
		}
		return findChildByType(JSTokenTypes.SET_KEYWORD) != null;
	}

	@RequiredReadAction
	@Override
	public boolean isConstructor()
	{
		final JSFunctionStub stub = getStub();
		if(stub != null)
		{
			return stub.isConstructor();
		}
		final PsiElement parent = JSResolveUtil.findParent(this);
		if(parent instanceof JSClass && Comparing.equal("constructor", getName(), true))
		{
			return true;
		}
		return false;
	}

	@Override
	public JSAttributeList getAttributeList()
	{
		return getStubOrPsiChild(JSElementTypes.ATTRIBUTE_LIST);
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSFunctionDeclaration(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		getNode().getTreeParent().removeChild(getNode());
	}

	@Override
	public String getQualifiedName()
	{
		final JSFunctionStub jsFunctionStub = getStub();
		if(jsFunctionStub != null)
		{
			return jsFunctionStub.getQualifiedName();
		}
		final PsiElement parent = JSResolveUtil.findParent(this);

		if(parent instanceof JSFile || parent instanceof JSPackageStatement)
		{
			return JSPsiImplUtils.getQName(this);
		}
		else
		{
			return getName();
		}
	}

	@Override
	@NotNull
	public SearchScope getUseScope()
	{
		if(isConstructor())
		{
			return super.getUseScope();
		}
		return JSResolveUtil.findUseScope(this);
	}

	@Override
	public PsiElement getNavigationElement()
	{
		PsiElement parent = getParent();
		if(parent instanceof JSClass)
		{
			PsiElement parentOriginalElement = parent.getNavigationElement();

			if(parentOriginalElement != parent)
			{
				JSFunction functionByNameAndKind = ((JSClass) parentOriginalElement).findFunctionByNameAndKind(getName(), getKind());
				return functionByNameAndKind != null ? functionByNameAndKind : this;
			}
		}
		return JSPsiImplUtils.findTopLevelNavigatableElement(this);
	}

	@Override
	public PsiElement setName(@NotNull String name) throws IncorrectOperationException
	{
		String oldName = getName();
		PsiElement element = super.setName(name);
		if(getParent() instanceof JSPackageStatement)
		{
			JSPsiImplUtils.updateFileName(this, name, oldName);
		}
		return element;
	}
}