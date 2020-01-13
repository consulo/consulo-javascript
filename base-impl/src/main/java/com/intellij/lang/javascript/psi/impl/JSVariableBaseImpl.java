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

import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.stubs.JSVariableStubBase;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.util.RecursionManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.SearchScope;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.lang.psi.JavaScriptType;
import consulo.javascript.lang.psi.JavaScriptTypeElement;

import javax.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 8:47:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSVariableBaseImpl<T extends JSVariableStubBase<T2>, T2 extends JSVariable> extends JSStubElementImpl<T> implements JSVariable
{
	protected JSVariableBaseImpl(ASTNode node)
	{
		super(node);
	}

	protected JSVariableBaseImpl(final T stub, final JSStubElementType<T, T2> elementType)
	{
		super(stub, elementType);
	}

	@Override
	public boolean hasInitializer()
	{
		return getInitializerText() != null;
	}

	@RequiredReadAction
	@Override
	public JSExpression getInitializer()
	{
		PsiElement element = findChildByType(JSTokenTypes.EQ);
		if(element == null)
		{
			return null;
		}

		while((element = element.getNextSibling()) != null)
		{
			if(element instanceof JSExpression)
			{
				return (JSExpression) element;
			}
		}
		return null;
	}

	@Override
	public String getInitializerText()
	{
		final T stub = getStub();
		if(stub != null)
		{
			return stub.getInitializerText();
		}

		final JSExpression expression = getInitializer();
		return expression != null ? expression.getText() : null;
	}

	@Override
	@Nonnull
	public SearchScope getUseScope()
	{
		return JSResolveUtil.findUseScope(this);
	}

	@Override
	public String getName()
	{
		final T stub = getStub();
		if(stub != null)
		{
			return stub.getName();
		}
		final PsiElement name = getNameIdentifier();

		if(name != null)
		{
			if(name instanceof JSReferenceExpression)
			{
				return ((JSReferenceExpression) name).getReferencedName();
			}
		}
		return name != null ? name.getText() : "";
	}

	@Override
	public JSAttributeList getAttributeList()
	{
		PsiElement parent = getParent();
		if(!(parent instanceof JSVarStatement))
		{
			return null;
		}
		return ((JSVarStatementImpl) getParent()).getStubOrPsiChild(JSElementTypes.ATTRIBUTE_LIST);
	}

	@Override
	public void setInitializer(JSExpression expr) throws IncorrectOperationException
	{
		throw new UnsupportedOperationException("TODO: implement");
	}

	@Nonnull
	@Override
	public JavaScriptType getType()
	{
		final JSExpression initializer = getInitializer();
		if(initializer != null)
		{
			JavaScriptType javaScriptType = RecursionManager.doPreventingRecursion(this, false, new Computable<JavaScriptType>()
			{
				@Override
				@RequiredReadAction
				public JavaScriptType compute()
				{
					return initializer.getType();
				}
			});
			return javaScriptType == null ? JavaScriptType.UNKNOWN : javaScriptType;
		}
		return JavaScriptType.UNKNOWN;
	}

	@Override
	public String getTypeString()
	{
		final T stub = getStub();
		if(stub != null)
		{
			return stub.getTypeString();
		}
		return doGetType();
	}

	@RequiredReadAction
	@Override
	public JavaScriptTypeElement getTypeElement()
	{
		return JSPsiImplUtils.findTypeElement(this);
	}

	protected String doGetType()
	{
		return JSPsiImplUtils.getType(this);
	}

	@Override
	public PsiElement setName(@Nonnull String name) throws IncorrectOperationException
	{
		final PsiElement nameNode = getNameIdentifier();
		if(nameNode == null)
		{
			return this;
		}
		final ASTNode nameElement = JSChangeUtil.createNameIdentifier(getProject(), name);
		getNode().replaceChild(nameNode.getNode(), nameElement);
		return this;
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSVariable(this);
	}

	@RequiredReadAction
	@Override
	public int getTextOffset()
	{
		final PsiElement name = getNameIdentifier();
		return name != null ? name.getTextOffset() : super.getTextOffset();
	}

	@Override
	public boolean isConst()
	{
		final T stub = getStub();
		if(stub != null)
		{
			return stub.isConst();
		}
		final ASTNode parent = getNode().getTreeParent();

		if(parent.getElementType() == JSElementTypes.VAR_STATEMENT)
		{
			ASTNode node = parent.getFirstChildNode();
			IElementType type = node.getElementType();

			if(type == JSElementTypes.ATTRIBUTE_LIST || type == JSElementTypes.REFERENCE_EXPRESSION)
			{
				node = node.getTreeNext();

				if(node != null && node.getElementType() == JSTokenTypes.WHITE_SPACE)
				{
					node = node.getTreeNext();
				}
			}
			return node != null && node.getElementType() == JSTokenTypes.CONST_KEYWORD;
		}

		return false;
	}

	@Override
	public boolean isLocal()
	{
		final T stub = getStub();
		if(stub != null)
		{
			return stub.isLocal();
		}
		final ASTNode parent = getNode().getTreeParent();
		return parent.getElementType() == JSElementTypes.VAR_STATEMENT && parent.getFirstChildNode().getElementType() == JSTokenTypes.LET_KEYWORD;
	}

	@Override
	public boolean isDeprecated()
	{
		final T stub = getStub();
		if(stub != null)
		{
			return stub.isDeprecated();
		}
		return JSDocumentationUtils.calculateDeprecated(this);
	}

	@Override
	public void delete() throws IncorrectOperationException
	{
		final ASTNode myNode = getNode();
		final ASTNode parent = myNode.getTreeParent();

		if(parent.getElementType() == JSElementTypes.VAR_STATEMENT)
		{
			final JSVariable[] jsVariables = ((JSVarStatement) parent.getPsi()).getVariables();

			if(jsVariables.length == 1)
			{
				parent.getPsi().delete();
			}
			else
			{
				JSChangeUtil.removeRangeWithRemovalOfCommas(myNode, parent);
			}
			return;
		}

		throw new IncorrectOperationException("Cannot delete variable from parent : " + parent.getElementType());
	}

	@Override
	public boolean processDeclarations(@Nonnull final PsiScopeProcessor processor, @Nonnull final ResolveState state, final PsiElement lastParent,
			@Nonnull final PsiElement place)
	{
		if(lastParent != null && lastParent.getParent() == this)
		{
			processor.handleEvent(PsiScopeProcessor.Event.SET_DECLARATION_HOLDER, this);
		}
		return processor.execute(this, state);
	}

	@Override
	public String getQualifiedName()
	{
		final T stub = getStub();
		if(stub != null)
		{
			return stub.getQualifiedName();
		}
		return JSPsiImplUtils.getQName(this);
	}

	@Override
	@RequiredReadAction
	public PsiElement getNameIdentifier()
	{
		return findChildByType(JavaScriptTokenSets.NAME_TOKEN_TYPES);
	}
}
