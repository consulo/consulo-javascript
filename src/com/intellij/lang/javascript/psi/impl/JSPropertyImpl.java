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
import org.mustbe.consulo.javascript.lang.JavaScriptTokenSets;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
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
import com.intellij.psi.util.PsiUtilCore;
import com.intellij.util.IncorrectOperationException;

/**
 * @author max
 */
public class JSPropertyImpl extends JSElementImpl implements JSProperty
{
	private static TokenSet IDENTIFIER_TOKENS_SET = TokenSet.orSet(JSTokenTypes.IDENTIFIER_TOKENS_SET, TokenSet.create(JSTokenTypes.NUMERIC_LITERAL), JavaScriptTokenSets.STRING_LITERALS);

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
			return new PsiReference[]{reference};
		}
		return PsiReference.EMPTY_ARRAY;
	}

	@Override
	@RequiredReadAction
	public String getName()
	{
		final PsiElement nameIdentifier = getNameIdentifier();
		if(nameIdentifier != null)
		{
			return StringUtil.stripQuotesAroundValue(nameIdentifier.getText());
		}
		return null;
	}

	@RequiredReadAction
	static boolean isGetIdentifier(final PsiElement node)
	{
		final IElementType type = PsiUtilCore.getElementType(node);

		return type == JSTokenTypes.GET_KEYWORD;
	}

	@RequiredReadAction
	static boolean isSetIdentifier(final PsiElement node)
	{
		final IElementType type = PsiUtilCore.getElementType(node);

		return type == JSTokenTypes.SET_KEYWORD;
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
	public PsiElement setName(@NotNull String name) throws IncorrectOperationException
	{
		final PsiElement nameNode = getNameIdentifier();
		assert nameNode != null;
		final ASTNode nameElement = JSChangeUtil.createNameIdentifier(getProject(), name, nameNode.getNode().getElementType());
		getNode().replaceChild(nameNode.getNode(), nameElement);
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

	@RequiredReadAction
	@Override
	public int getTextOffset()
	{
		final PsiElement name = getNameIdentifier();
		return name != null ? name.getTextOffset() : super.getTextOffset();
	}

	@Override
	@RequiredReadAction
	public void delete() throws IncorrectOperationException
	{
		final ASTNode myNode = getNode();
		JSChangeUtil.removeRangeWithRemovalOfCommas(myNode, myNode.getTreeParent());
	}

	@Override
	@RequiredReadAction
	public PsiElement getNameIdentifier()
	{
		return findChildByType(IDENTIFIER_TOKENS_SET);
	}
}
