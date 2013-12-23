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

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.icons.AllIcons;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.resolve.VariantsProcessor;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiReference;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import com.intellij.util.IncorrectOperationException;

/**
 * @by max
 */
public class JSPropertyImpl extends JSElementImpl implements JSProperty
{
	private static TokenSet IDENTIFIER_TOKENS_SET = TokenSet.orSet(JSTokenTypes.IDENTIFIER_TOKENS_SET, TokenSet.create(JSTokenTypes.STRING_LITERAL,
			JSTokenTypes.NUMERIC_LITERAL));

	private static TokenSet FUNCTION_TOKEN_SET = TokenSet.create(JSElementTypes.FUNCTION_DECLARATION);

	private PsiReference[] myReferences;
	private String myReferenceText;

	public JSPropertyImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	@NotNull
	public PsiReference[] getReferences()
	{
		final String text = getText();

		if(myReferences != null &&
				myReferenceText != null &&
				myReferenceText.equals(text))
		{
			return myReferences;
		}

		myReferenceText = text;
		return myReferences = new PsiReference[]{new PropertyNameReference()};
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

	public Icon getIcon(int flags)
	{
		return AllIcons.Nodes.Property;
	}

	private class PropertyNameReference implements PsiReference
	{
		@Override
		public PsiElement getElement()
		{
			return getFirstChild();
		}

		@Override
		public TextRange getRangeInElement()
		{
			final PsiElement firstChild = getFirstChild();
			int quotesDelta = firstChild.getNode().getElementType() == JSTokenTypes.STRING_LITERAL ? 1 : 0;
			final int startOffsetInParent = firstChild.getStartOffsetInParent();
			return new TextRange(startOffsetInParent + quotesDelta, startOffsetInParent + firstChild.getTextLength() - quotesDelta);
		}

		@Override
		@Nullable
		public PsiElement resolve()
		{
			return JSPropertyImpl.this;
		}

		@Override
		public String getCanonicalText()
		{
			return StringUtil.stripQuotesAroundValue(getFirstChild().getText());
		}

		@Override
		public PsiElement handleElementRename(String newElementName) throws IncorrectOperationException
		{
			setName(newElementName);
			return null;
		}

		@Override
		public PsiElement bindToElement(@NotNull PsiElement element) throws IncorrectOperationException
		{
			return null;
		}

		@Override
		public boolean isReferenceTo(PsiElement element)
		{
			final PsiElement element2 = resolve();
			boolean proxyExpanded = false;

			if(element instanceof JSDefinitionExpression)
			{
				final JSExpression expression = ((JSDefinitionExpression) element).getExpression();
				if(expression instanceof JSReferenceExpression)
				{
					return ((JSReferenceExpression) expression).isReferenceTo(element2);
				}
			}

			if(element instanceof JSNamedElementProxy)
			{
				element = ((JSNamedElementProxy) element).getElement();
				proxyExpanded = true;
			}

			if(element != element2 && element instanceof JSProperty && element2 instanceof JSProperty)
			{
				return ((JSProperty) element).getName().equals(((JSProperty) element2).getName());
			}
			return proxyExpanded && element == element2;
		}

		@Override
		public Object[] getVariants()
		{
			final VariantsProcessor processor = new VariantsProcessor(null, getContainingFile(), false, JSPropertyImpl.this);
			JavaScriptIndex.getInstance(getProject()).processAllSymbols(processor);

			return processor.getResult();
		}

		@Override
		public boolean isSoft()
		{
			return true;
		}
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
