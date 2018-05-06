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

import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSSourceElement;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.psi.PsiElement;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.util.IncorrectOperationException;
import consulo.annotations.RequiredReadAction;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.lang.psi.JavaScriptType;
import consulo.javascript.lang.psi.JavaScriptTypeElement;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 8:25:27 PM
 */
abstract class JSFunctionBaseImpl<T extends JSFunctionStub, T2 extends JSFunction> extends JSStubElementImpl<T> implements JSFunction
{
	private boolean referencesArgumentsCalculated;
	private boolean referencesArguments;

	public JSFunctionBaseImpl(final ASTNode node)
	{
		super(node);
	}

	public JSFunctionBaseImpl(final T stub, IStubElementType type)
	{
		super(stub, type);
	}

	@Override
	public void subtreeChanged()
	{
		super.subtreeChanged();
		referencesArgumentsCalculated = false;
		referencesArguments = false;
	}

	@RequiredReadAction
	@Override
	public JSParameterList getParameterList()
	{
		return getStubOrPsiChild(JSElementTypes.PARAMETER_LIST);
	}

	@Override
	public JSSourceElement[] getBody()
	{
		final ASTNode[] children = getNode().getChildren(JSElementTypes.SOURCE_ELEMENTS);
		if(children.length == 0)
		{
			return JSSourceElement.EMPTY_ARRAY;
		}
		JSSourceElement[] result = new JSSourceElement[children.length];
		for(int i = 0; i < children.length; i++)
		{
			result[i] = (JSSourceElement) children[i].getPsi();
		}
		return result;
	}

	@Nonnull
	@Override
	public JavaScriptType getReturnType()
	{
		return JavaScriptType.UNKNOWN;
	}

	@Override
	public String getReturnTypeString()
	{
		final T stub = getStub();
		if(stub != null)
		{
			return stub.getReturnTypeString();
		}
		return JSPsiImplUtils.getType(this);
	}

	@Override
	public JavaScriptTypeElement getReturnTypeElement()
	{
		return JSPsiImplUtils.findTypeElement(this);
	}

	@Override
	public PsiElement setName(@Nonnull String name) throws IncorrectOperationException
	{
		final ASTNode newNameElement = createNameIdentifier(name);
		final ASTNode nameIdentifier = getNameIdentifier().getNode();
		nameIdentifier.getTreeParent().replaceChild(nameIdentifier, newNameElement);
		return this;
	}

	protected ASTNode createNameIdentifier(final String name)
	{
		return JSChangeUtil.createExpressionFromText(getProject(), name).getNode();
	}

	@Override
	@RequiredReadAction
	public String getName()
	{
		final JSFunctionStub stub = getStub();
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
			else
			{
				return name.getText();
			}
		}
		return null;
	}

	private static ASTNode advance(ASTNode astNode)
	{
		astNode = astNode != null ? astNode.getTreeNext() : null;

		if(astNode != null && astNode.getElementType() == JSTokenTypes.WHITE_SPACE)
		{
			astNode = astNode.getTreeNext();
		}
		return astNode;
	}

	@RequiredReadAction
	@Override
	public int getTextOffset()
	{
		final PsiElement name = getNameIdentifier();
		return name != null ? name.getTextOffset() : super.getTextOffset();
	}

	@Override
	public boolean processDeclarations(@Nonnull PsiScopeProcessor processor, @Nonnull ResolveState state, PsiElement lastParent,
			@Nonnull PsiElement place)
	{
		if(lastParent != null && lastParent.getParent() == this)
		{
			if(place instanceof JSReferenceExpression)
			{
				boolean b = JSImportHandlingUtil.tryResolveImports(processor, this, place);
				if(!b || JSResolveUtil.isExprInStrictTypeContext((JSReferenceExpression) place))
				{
					return b;
				}
			}

			final JSParameter[] params = getParameterList().getParameters();
			for(JSParameter param : params)
			{
				if(!processor.execute(param, state))
				{
					return false;
				}
			}

			boolean b = JSResolveUtil.processDeclarationsInScope(this, processor, state, lastParent, place);
			if(b)
			{
				processor.handleEvent(PsiScopeProcessor.Event.SET_DECLARATION_HOLDER, this);
			}
			return b;
		}

		return processor.execute(this, state);
	}

	@Override
	public PsiElement addBefore(@Nonnull final PsiElement element, final PsiElement anchor) throws IncorrectOperationException
	{
		if(anchor == getFirstChild() && element instanceof JSAttributeList && anchor.getNode().getElementType() == JSTokenTypes.FUNCTION_KEYWORD)
		{
			return JSChangeUtil.doDoAddBefore(this, element, anchor);
		}
		return super.addBefore(element, anchor);
	}

	@Override
	public FunctionKind getKind()
	{
		if(isGetProperty())
		{
			return FunctionKind.GETTER;
		}
		if(isSetProperty())
		{
			return FunctionKind.SETTER;
		}
		if(isConstructor())
		{
			return FunctionKind.CONSTRUCTOR;
		}
		return FunctionKind.SIMPLE;
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
	public boolean isReferencesArguments()
	{
		final T stub = getStub();
		if(stub != null)
		{
			return stub.isReferencesArguments();
		}

		if(!referencesArgumentsCalculated)
		{
			acceptChildren(new JSElementVisitor()
			{
				boolean continueVisiting = true;

				@Override
				public void visitJSReferenceExpression(final JSReferenceExpression node)
				{
					if(isInJS(node) && node.getQualifier() == null)
					{
						if("arguments".equals(node.getText()))
						{
							referencesArguments = true;
							continueVisiting = false;
							return;
						}
					}
					super.visitJSReferenceExpression(node);
				}

				@Override
				public void visitJSElement(final JSElement node)
				{
					if(continueVisiting)
					{
						node.acceptChildren(this);
					}
				}
			});

			referencesArgumentsCalculated = true;
		}

		return referencesArguments;
	}

	private static boolean isInJS(final JSReferenceExpression node)
	{
		final PsiElement parent = node.getParent();
		if(parent != null && !(parent.getLanguage() instanceof JavaScriptLanguage))
		{
			return false;
		}
		return true;
	}

	@Override
	@RequiredReadAction
	public PsiElement getNameIdentifier()
	{
		return findChildByType(JavaScriptTokenSets.NAME_TOKEN_TYPES);
	}
}
