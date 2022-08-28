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

package com.intellij.lang.javascript.impl.search;

import com.intellij.lang.javascript.psi.JSNamedElement;
import consulo.document.util.TextRange;
import consulo.language.editor.hint.DeclarationRangeHandler;
import consulo.language.psi.PsiElement;

import javax.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 28, 2008
 *         Time: 8:36:19 PM
 */
public abstract class JSDeclarationRangeHandler implements DeclarationRangeHandler
{
	@Override
	@Nonnull
	public TextRange getDeclarationRange(@Nonnull PsiElement container)
	{
		JSNamedElement namedElement = (JSNamedElement) container;

		final TextRange textRange = namedElement.getTextRange();
		final PsiElement nameIdentifier = namedElement.getNameIdentifier();
		final TextRange nameIdentifierRange = nameIdentifier != null ? nameIdentifier.getTextRange() : null;
		int startOffset = nameIdentifierRange != null ? nameIdentifierRange.getStartOffset() : textRange.getStartOffset();
		int endOffset = nameIdentifierRange != null ? nameIdentifierRange.getEndOffset() : textRange.getEndOffset();

		return new TextRange(startOffset, endOffset);
	}
}
