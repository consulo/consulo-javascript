/*
 * Copyright 2013-2015 must-be.org
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

package consulo.javascript.lang;

import org.jetbrains.annotations.NotNull;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageUtil;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.impl.JSDocCommentImpl;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.impl.JSFileImpl;
import com.intellij.lexer.Lexer;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import consulo.lang.LanguageVersionableParserDefinition;

/**
 * @author VISTALL
 * @since 24.08.14
 */
public class JavaScriptParsingDefinition extends LanguageVersionableParserDefinition
{
	@NotNull
	@Override
	public IFileElementType getFileNodeType()
	{
		return JSElementTypes.FILE;
	}

	@NotNull
	@Override
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

	@Override
	public PsiFile createFile(FileViewProvider fileViewProvider)
	{
		return new JSFileImpl(fileViewProvider);
	}

	@NotNull
	@Override
	public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right)
	{
		PsiElement leftPsi = left.getPsi();
		final Lexer lexer = createLexer(leftPsi.getLanguageVersion());
		return LanguageUtil.canStickTokensTogetherByLexer(left, right, lexer);
	}
}
