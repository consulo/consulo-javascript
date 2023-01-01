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

package consulo.javascript.impl.lang;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.impl.JSDocCommentImpl;
import com.intellij.lang.javascript.psi.impl.JSEmbeddedContentImpl;
import com.intellij.lang.javascript.psi.impl.JSFileImpl;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.IFileElementType;
import consulo.language.file.FileViewProvider;
import consulo.language.impl.psi.ASTWrapperPsiElement;
import consulo.language.lexer.Lexer;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.util.LanguageUtil;
import consulo.language.version.LanguageVersionableParserDefinition;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 24.08.14
 */
@ExtensionImpl
public class JavaScriptParsingDefinition extends LanguageVersionableParserDefinition
{
	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}

	@Nonnull
	@Override
	public IFileElementType getFileNodeType()
	{
		return JSElementTypes.FILE;
	}

	@Nonnull
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

	@Nonnull
	@Override
	public SpaceRequirements spaceExistanceTypeBetweenTokens(ASTNode left, ASTNode right)
	{
		PsiElement leftPsi = left.getPsi();
		final Lexer lexer = createLexer(leftPsi.getLanguageVersion());
		return LanguageUtil.canStickTokensTogetherByLexer(left, right, lexer);
	}
}
