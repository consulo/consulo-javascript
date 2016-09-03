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
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import consulo.javascript.psi.impl.reference.JSPropertyNameReferenceProvider;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.tree.TokenSet;
import com.intellij.psi.util.CachedValueProvider;
import com.intellij.psi.util.CachedValuesManager;
import com.intellij.util.IncorrectOperationException;
import consulo.annotations.RequiredReadAction;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.lang.psi.JavaScriptType;
import consulo.javascript.psi.JSComputedName;

/**
 * @author max
 */
public class JSPropertyImpl extends JSElementImpl implements JSProperty
{
	private static TokenSet IDENTIFIER_TOKENS_SET = TokenSet.orSet(TokenSet.create(JSTokenTypes.NUMERIC_LITERAL, JSTokenTypes.IDENTIFIER), JavaScriptTokenSets.STRING_LITERALS);

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
	@NotNull
	@Override
	public JavaScriptType getType()
	{
		JSExpression value = getValue();
		if(value != null)
		{
			return value.getType();
		}
		return JavaScriptType.UNKNOWN;
	}

	@RequiredReadAction
	@Override
	public JSExpression getValue()
	{
		return findChildByClass(JSExpression.class);
	}

	@RequiredReadAction
	@Nullable
	@Override
	public PsiElement getColonElement()
	{
		return findChildByType(JSTokenTypes.COLON);
	}

	@RequiredReadAction
	@Nullable
	@Override
	public JSComputedName getComputedName()
	{
		return findChildByClass(JSComputedName.class);
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
		PsiElement nameIdentifier = findChildByType(IDENTIFIER_TOKENS_SET);
		if(nameIdentifier == null)
		{
			JSExpression value = getValue();
			// auto-property name
			if(value != null && getColonElement() == null)
			{
				return value;
			}
		}
		return nameIdentifier;
	}
}
