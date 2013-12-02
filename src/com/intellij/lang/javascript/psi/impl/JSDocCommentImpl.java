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
import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.psi.JSDocComment;
import com.intellij.lang.javascript.psi.JSDocTag;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.tree.IElementType;

public class JSDocCommentImpl extends JSElementImpl implements JSDocComment
{
	public JSDocCommentImpl(final ASTNode node)
	{
		super(node);
	}

	public IElementType getTokenType()
	{
		return getNode().getElementType();
	}

	@Override
	public void accept(@NotNull final PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSDocComment(this);
		}
		else
		{
			visitor.visitComment(this);
		}
	}

	public JSDocTag[] getTags()
	{
		final PsiElement firstChild = getFirstChild();
		if(firstChild instanceof JSDocComment)
		{
			return ((JSDocComment) firstChild).getTags();
		}
		return findChildrenByType(JSDocTokenTypes.DOC_TAG, JSDocTag.class);
	}
}