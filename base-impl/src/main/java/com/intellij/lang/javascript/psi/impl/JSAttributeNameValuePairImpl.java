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

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttribute;
import com.intellij.lang.javascript.psi.JSAttributeNameValuePair;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.stubs.JSAttributeNameValuePairStub;
import consulo.language.psi.PsiReference;
import consulo.language.util.IncorrectOperationException;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.language.psi.PsiElement;
import consulo.util.lang.StringUtil;
import org.jetbrains.annotations.NonNls;

import jakarta.annotation.Nonnull;

/**
 * @by Maxim.Mossienko
 */
public class JSAttributeNameValuePairImpl extends JSStubElementImpl<JSAttributeNameValuePairStub> implements JSAttributeNameValuePair
{
	private JSReferenceSet myReferenceSet;
	@NonNls
	private static final String EMBED_ANNOTATION_NAME = "Embed";

	public JSAttributeNameValuePairImpl(final ASTNode node)
	{
		super(node);
	}

	public JSAttributeNameValuePairImpl(final JSAttributeNameValuePairStub node)
	{
		super(node, JSElementTypes.ATTRIBUTE_NAME_VALUE_PAIR);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSAttributeNameValuePair(this);
	}

	@Override
	public String getName()
	{
		final JSAttributeNameValuePairStub stub = getStub();
		if(stub != null)
		{
			return stub.getName();
		}
		final ASTNode node = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
		return node != null ? node.getText() : null;
	}

	@Override
	public PsiElement setName(@NonNls @Nonnull final String name) throws IncorrectOperationException
	{
		throw new IncorrectOperationException();
	}

	@Override
	public JSExpression getValue()
	{
		final ASTNode astNode = findValueNode();
		return astNode != null ? (JSExpression) astNode.getPsi() : null;
	}

	@Override
	public String getSimpleValue()
	{
		final JSAttributeNameValuePairStub stub = getStub();
		if(stub != null)
		{
			return stub.getValue();
		}
		final ASTNode expression = findValueNode();
		return expression != null ? StringUtil.stripQuotesAroundValue(expression.getText()) : null;
	}

	private ASTNode findValueNode()
	{
		return getNode().findChildByType(JavaScriptTokenSets.STRING_LITERALS);
	}

	@Override
	@Nonnull
	public PsiReference[] getReferences()
	{
		final @NonNls String name = getName();

		if("source".equals(name))
		{
			return getPathRefsCheckingParent();
		}
		else if("type".equals(name) || "arrayType".equals(name))
		{
			return getClassRefs();
		}
		else if(name == null)
		{
			return getDefaultPropertyRefs();
		}
		return PsiReference.EMPTY_ARRAY;
	}

	private PsiReference[] getClassRefs()
	{
		final ASTNode valueNode = findValueNode();

		if(valueNode != null)
		{
			if(myReferenceSet == null)
			{
				myReferenceSet = new JSReferenceSet(this, false);
			}
			myReferenceSet.update(valueNode.getText(), valueNode.getPsi().getStartOffsetInParent());
			return myReferenceSet.getReferences();
		}
		return PsiReference.EMPTY_ARRAY;
	}

	private PsiReference[] getDefaultPropertyRefs()
	{
		final @NonNls String parentName = ((JSAttribute) getParent()).getName();

		if("HostComponent".equals(parentName) || "ArrayElementType".equals(parentName))
		{
			return getClassRefs();
		}

		if(EMBED_ANNOTATION_NAME.equals(parentName))
		{
			return getPathRefs();
		}
		if("DefaultProperty".equals(parentName))
		{
			final ASTNode valueNode = findValueNode();
			if(valueNode != null)
			{
				if(myReferenceSet == null)
				{
					myReferenceSet = new JSReferenceSet(this, false);
				}
				myReferenceSet.update(valueNode.getText(), valueNode.getPsi().getStartOffsetInParent());
				return myReferenceSet.getReferences();
			}
		}

		return PsiReference.EMPTY_ARRAY;
	}

	private PsiReference[] getPathRefsCheckingParent()
	{
		final @NonNls String parentName = ((JSAttribute) getParent()).getName();

		if(!EMBED_ANNOTATION_NAME.equals(parentName))
		{
			return PsiReference.EMPTY_ARRAY;
		}
		return getPathRefs();
	}

	private PsiReference[] getPathRefs()
	{
		final ASTNode valueNode = findValueNode();

		if(valueNode != null)
		{
			return ReferenceSupport.getFileRefs(this, valueNode.getPsi(), valueNode.getPsi().getStartOffsetInParent() + 1,
					ReferenceSupport.LookupOptions.EMBEDDED_ASSET);
		}
		return PsiReference.EMPTY_ARRAY;
	}
}
