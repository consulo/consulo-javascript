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
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSIncludeDirective;
import com.intellij.lang.javascript.psi.stubs.JSIncludeDirectiveStub;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.impl.source.resolve.reference.impl.providers.FileReference;
import com.intellij.psi.impl.source.resolve.reference.impl.providers.FileReferenceSet;

/**
 * @by Maxim.Mossienko
 */
public class JSIncludeDirectiveImpl extends JSStubbedStatementImpl<JSIncludeDirectiveStub> implements JSIncludeDirective
{
	public JSIncludeDirectiveImpl(final ASTNode node)
	{
		super(node);
	}

	public JSIncludeDirectiveImpl(final JSIncludeDirectiveStub stub)
	{
		super(stub, JSElementTypes.INCLUDE_DIRECTIVE);
	}

	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSIncludeDirective(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@NotNull
	public PsiReference[] getReferences()
	{
		ASTNode node = getIncludedFileNode();

		if(node != null)
		{
			return new FileReferenceSet(StringUtil.stripQuotesAroundValue(node.getText()), this, node.getPsi().getStartOffsetInParent() + 1, null,
					SystemInfo.isFileSystemCaseSensitive).getAllReferences();
		}
		return PsiReference.EMPTY_ARRAY;
	}

	private ASTNode getIncludedFileNode()
	{
		ASTNode node = getNode().findChildByType(JSTokenTypes.STRING_LITERAL);
		if(node == null)
		{
			getNode().findChildByType(JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL);
		}
		return node;
	}

	public String getIncludeText()
	{
		final JSIncludeDirectiveStub stub = getStub();
		if(stub != null)
		{
			return stub.getIncludeText();
		}
		final ASTNode astNode = getIncludedFileNode();

		return astNode != null ? StringUtil.stripQuotesAroundValue(astNode.getText()) : null;
	}

	public PsiFile resolveFile()
	{
		final String includeText = getIncludeText();
		if(includeText == null)
		{
			return null;
		}
		final FileReference[] references = new FileReferenceSet(includeText, this, 0, null, SystemInfo.isFileSystemCaseSensitive).getAllReferences();

		if(references != null && references.length > 0)
		{
			final PsiElement element = references[references.length - 1].resolve();
			if(element instanceof PsiFile)
			{
				return (PsiFile) element;
			}
		}
		return null;
	}
}