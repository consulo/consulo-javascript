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

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiElementProcessor;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import consulo.xml.psi.xml.XmlTag;
import consulo.xml.psi.xml.XmlTagChild;

import javax.annotation.Nonnull;

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
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSElement(this);
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
