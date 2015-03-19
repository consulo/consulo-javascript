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

package com.intellij.lang.javascript;

import org.jetbrains.annotations.NotNull;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageUtil;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.ParserDefinition;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.parsing.JSParser;
import com.intellij.lang.javascript.psi.impl.JSDocCommentImpl;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.impl.JSFileImpl;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.psi.tree.TokenSet;

/**
 * @author max
 */
@Deprecated
public class JavascriptParserDefinition implements ParserDefinition
{
	@Override
	@NotNull
	public Lexer createLexer(Project project, LanguageVersion languageVersion)
	{
		return new JavaScriptParsingLexer(JavascriptLanguage.DIALECT_OPTION_HOLDER);
	}

	@NotNull
	@Override
	public IFileElementType getFileNodeType()
	{
		return JSElementTypes.FILE;
	}

	@Override
	@NotNull
	public TokenSet getWhitespaceTokens(LanguageVersion languageVersion)
	{
		return TokenSet.create(JSTokenTypes.WHITE_SPACE);
	}

	@Override
	@NotNull
	public TokenSet getCommentTokens(LanguageVersion languageVersion)
	{
		return JSTokenTypes.COMMENTS;
	}

	@Override
	@NotNull
	public TokenSet getStringLiteralElements(LanguageVersion languageVersion)
	{
		return TokenSet.EMPTY;
	}

	@Override
	@NotNull
	public PsiParser createParser(final Project project, LanguageVersion languageVersion)
	{
		return new JSParser(null);
	}

	@Override
	public PsiFile createFile(FileViewProvider viewProvider)
	{
		return new JSFileImpl(viewProvider);
	}

	@NotNull
	@Override
	public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right)
	{
		final Lexer lexer = createLexer(left.getPsi().getProject(), null);
		return LanguageUtil.canStickTokensTogetherByLexer(left, right, lexer);
	}

	@Override
	@NotNull
	public PsiElement createElement(ASTNode node)
	{
		final IElementType type = node.getElementType();

		if(type == JSElementTypes.EMBEDDED_CONTENT)
		{
			return new JSEmbeddedContentImpl(node);
		}
		else if(type == JSTokenTypes.XML_JS_SCRIPT)
		{
			return new JSEmbeddedContentImpl(node);
		}
		else if(type == JSTokenTypes.DOC_COMMENT)
		{
			return new JSDocCommentImpl(node);
		}

		return new ASTWrapperPsiElement(node);
	}
}
