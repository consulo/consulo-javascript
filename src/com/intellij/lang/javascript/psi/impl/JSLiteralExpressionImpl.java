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
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSSimpleLiteralExpression;
import com.intellij.psi.LiteralTextEscaper;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiLanguageInjectionHost;
import com.intellij.psi.PsiReference;
import com.intellij.psi.tree.IElementType;
import consulo.annotations.RequiredReadAction;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.lang.psi.JavaScriptPrimitiveType;
import consulo.javascript.lang.psi.JavaScriptType;

/**
 * @author max
 * @since 11:24:42 PM Jan 30, 2005
 */
public class JSLiteralExpressionImpl extends JSExpressionImpl implements JSSimpleLiteralExpression, PsiLanguageInjectionHost
{
	private volatile JSReferenceSet myReferenceSet;
	private volatile long myModCount;

	public JSLiteralExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@RequiredReadAction
	@NotNull
	@Override
	public IElementType getLiteralElementType()
	{
		PsiElement firstChild = getFirstChild();
		assert firstChild != null;
		return firstChild.getNode().getElementType();
	}

	@RequiredReadAction
	@NotNull
	@Override
	public JavaScriptType getType()
	{
		IElementType literalElementType = getLiteralElementType();
		if(literalElementType == JSTokenTypes.TRUE_KEYWORD || literalElementType == JSTokenTypes.FALSE_KEYWORD)
		{
			return JavaScriptPrimitiveType.BOOL;
		}
		else if(literalElementType == JSTokenTypes.REGEXP_LITERAL)
		{
			return JavaScriptPrimitiveType.REGEXP;
		}
		else if(literalElementType == JSTokenTypes.NUMERIC_LITERAL)
		{
			return JavaScriptPrimitiveType.NUMBER;
		}
		else if(literalElementType == JSTokenTypes.NULL_KEYWORD)
		{
			return JavaScriptPrimitiveType.NULL;
		}
		else if(JavaScriptTokenSets.STRING_LITERALS.contains(literalElementType))
		{
			return JavaScriptPrimitiveType.STRING;
		}
		return super.getType();
	}

	@Override
	@NotNull
	public PsiReference[] getReferences()
	{
		JSReferenceSet referenceSet = myReferenceSet;

		if(referenceSet == null)
		{
			synchronized(this)
			{
				referenceSet = myReferenceSet;
				if(referenceSet == null)
				{
					referenceSet = new JSReferenceSet(this);
					referenceSet.update(getText(), 0);
					myReferenceSet = referenceSet;
				}
			}
		}
		else
		{
			final long count = getManager().getModificationTracker().getModificationCount();

			if(count != myModCount)
			{
				synchronized(this)
				{
					if(count != myModCount)
					{
						referenceSet.update(getText(), 0);
						myModCount = count;
					}
				}
			}
		}

		return myReferenceSet.getReferences();
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSLiteralExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}

	@Override
	public boolean isValidHost()
	{
		return true;
	}

	@Override
	public PsiLanguageInjectionHost updateText(@NotNull String text)
	{
		JSExpression expressionFromText = JSChangeUtil.createExpressionFromText(getProject(), text);
		return (PsiLanguageInjectionHost) replace(expressionFromText);
	}

	@NotNull
	@Override
	public LiteralTextEscaper<? extends PsiLanguageInjectionHost> createLiteralTextEscaper()
	{
		return LiteralTextEscaper.createSimple(this);
	}
}
