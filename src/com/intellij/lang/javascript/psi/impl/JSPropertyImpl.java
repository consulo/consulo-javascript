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
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredReadAction;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.impl.reference.JSPropertyNameReferenceProvider;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.util.IncorrectOperationException;

/**
 * @author max
 */
public class JSPropertyImpl extends JSElementImpl implements JSProperty
{
	private static TokenSet IDENTIFIER_TOKENS_SET = TokenSet.orSet(JSTokenTypes.IDENTIFIER_TOKENS_SET, TokenSet.create(JSTokenTypes.STRING_LITERAL,
			JSTokenTypes.NUMERIC_LITERAL));

	private static TokenSet FUNCTION_TOKEN_SET = TokenSet.create(JSElementTypes.FUNCTION_DECLARATION);

	public JSPropertyImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	@NotNull
	public PsiReference[] getReferences()
	{
		return CachedValuesManager.getCachedValue(this, new CachedValueProvider<PsiReference[]>()
		{
			@Nullable
			@Override
			@RequiredReadAction
			public Result<PsiReference[]> compute()
			{
				return Result.create(buildReferences(), JSPropertyImpl.this);
			}
		});
	}

	@NotNull
	@RequiredReadAction
	private PsiReference[] buildReferences()
	{
		PsiElement nameIdentifier = getNameIdentifier();
		if(nameIdentifier == null)
		{
			return PsiReference.EMPTY_ARRAY;
		}
		PsiReference reference = JSPropertyNameReferenceProvider.EP_NAME.composite().getReference(this);
		if(reference != null)
		{
			return new PsiReference[] {reference};
		}
		return PsiReference.EMPTY_ARRAY;
	}

	@Override
	public String getName()
	{
		final ASTNode node = findNameIdentifier();
		if(node != null)
		{
			return StringUtil.stripQuotesAroundValue(node.getText());
		}
		return null;
	}

	@Override
	public ASTNode findNameIdentifier()
	{
		ASTNode node = getNode().findChildByType(IDENTIFIER_TOKENS_SET);
		if(node != null)
		{

			if(isGetIdentifier(node) || isSetIdentifier(node))
			{
				ASTNode prevNode = node;
				final ASTNode funcDeclNode = getNode().findChildByType(FUNCTION_TOKEN_SET, node);
				node = funcDeclNode != null ? ((JSFunction) funcDeclNode.getPsi()).findNameIdentifier() : null;
				if(node == null)
				{
					node = prevNode;
				}
			}
		}
		return node;
	}

	static boolean isGetIdentifier(final ASTNode node)
	{
		final IElementType type = node != null ? node.getElementType() : null;

		return type == JSTokenTypes.GET_KEYWORD || type == JSTokenTypes.IDENTIFIER && "get".equals(node.getText());
	}

	static boolean isSetIdentifier(final ASTNode node)
	{
		final IElementType type = node != null ? node.getElementType() : null;

		return type == JSTokenTypes.SET_KEYWORD || type == JSTokenTypes.IDENTIFIER && "set".equals(node.getText());
	}

	@Override
	public boolean isGetProperty()
	{
		final ASTNode node = getNode().findChildByType(IDENTIFIER_TOKENS_SET);
		return node != null && isGetIdentifier(node) && getNode().findChildByType(IDENTIFIER_TOKENS_SET, node) != null;
	}

	@Override
	public boolean isSetProperty()
	{
		final ASTNode node = getNode().findChildByType(IDENTIFIER_TOKENS_SET);
		return node != null && isSetIdentifier(node) && getNode().findChildByType(IDENTIFIER_TOKENS_SET, node) != null;
	}

	@Override
	public PsiElement setName(@NotNull String name) throws IncorrectOperationException
	{
		final ASTNode nameNode = findNameIdentifier();
		final ASTNode nameElement = JSChangeUtil.createNameIdentifier(getProject(), name, nameNode.getElementType());
		getNode().replaceChild(nameNode, nameElement);
		return this;
	}

	@RequiredReadAction
	@Override
	public JSExpression getValue()
	{
		final ASTNode node = getNode().findChildByType(JSElementTypes.EXPRESSIONS);
		return node != null ? (JSExpression) node.getPsi() : null;
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSProperty(this);
		}
		else
		{
			visitor.visitElement(this);
		}
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
		final ASTNode myNode = getNode();
		JSChangeUtil.removeRangeWithRemovalOfCommas(myNode, myNode.getTreeParent());
	}

	@Override
	public PsiElement getNameIdentifier()
	{
		final ASTNode node = findNameIdentifier();
		return node != null ? node.getPsi() : null;
	}
}
