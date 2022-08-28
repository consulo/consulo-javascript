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

package com.intellij.lang.javascript.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.CodeDocumentationAwareCommenter;
import consulo.language.Language;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiComment;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author max
 */
@ExtensionImpl
public class JavascriptCommenter implements CodeDocumentationAwareCommenter
{
	@Override
	public String getLineCommentPrefix()
	{
		return "//";
	}

	@Override
	public String getBlockCommentPrefix()
	{
		return "/*";
	}

	@Override
	public String getBlockCommentSuffix()
	{
		return "*/";
	}

	@Nullable
	@Override
	public String getCommentedBlockCommentPrefix()
	{
		return null;
	}

	@Nullable
	@Override
	public String getCommentedBlockCommentSuffix()
	{
		return null;
	}

	@Override
	@Nullable
	public IElementType getLineCommentTokenType()
	{
		return JSTokenTypes.END_OF_LINE_COMMENT;
	}

	@Override
	@Nullable
	public IElementType getBlockCommentTokenType()
	{
		return JSTokenTypes.C_STYLE_COMMENT;
	}

	@Override
	public String getDocumentationCommentPrefix()
	{
		return "/**";
	}

	@Override
	public String getDocumentationCommentLinePrefix()
	{
		return "*";
	}

	@Override
	public String getDocumentationCommentSuffix()
	{
		return "*/";
	}

	@Override
	public boolean isDocumentationComment(final PsiComment element)
	{
		return element.getTokenType() == JSTokenTypes.DOC_COMMENT;
	}

	@Override
	@Nullable
	public IElementType getDocumentationCommentTokenType()
	{
		return JSTokenTypes.DOC_COMMENT;
	}

	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}
}
