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

import javax.annotation.Nonnull;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.PsiElementProcessor;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.xml.XmlTag;
import com.intellij.psi.xml.XmlTagChild;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 31, 2005
 * Time: 12:02:38 AM
 * To change this template use File | Settings | File Templates.
 */
public class JSEmbeddedContentImpl extends JSElementImpl implements XmlTagChild
{
	public JSEmbeddedContentImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public void accept(@Nonnull PsiElementVisitor visitor)
	{
		visitor.visitElement(this);
	}

	@Override
	public XmlTag getParentTag()
	{
		final PsiElement parent = getParent();
		if(parent instanceof XmlTag)
		{
			return (XmlTag) parent;
		}
		return null;
	}

	@Override
	public XmlTagChild getNextSiblingInTag()
	{
		PsiElement nextSibling = getNextSibling();
		if(nextSibling instanceof XmlTagChild)
		{
			return (XmlTagChild) nextSibling;
		}
		return null;
	}

	@Override
	public XmlTagChild getPrevSiblingInTag()
	{
		final PsiElement prevSibling = getPrevSibling();
		if(prevSibling instanceof XmlTagChild)
		{
			return (XmlTagChild) prevSibling;
		}
		return null;
	}

	@Override
	public boolean processElements(PsiElementProcessor processor, PsiElement place)
	{
		// TODO
		return true;
	}

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent,
			@Nonnull PsiElement place)
	{
		return JSResolveUtil.processDeclarationsInScope(this, processor, state, lastParent, place);
	}

	@Override
	public String toString()
	{
		String s = super.toString();
		final IElementType type = getNode().getElementType();
		if(type != JSElementTypes.EMBEDDED_CONTENT)
		{
			s += ":" + type;
		}
		return s;
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		final ASTNode astNode = getNode();
		astNode.getTreeParent().removeChild(astNode);
	}
}
