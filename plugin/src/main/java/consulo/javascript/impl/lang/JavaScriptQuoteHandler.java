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

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.editor.action.JavaLikeQuoteHandler;
import consulo.language.editor.action.SimpleTokenSetQuoteHandler;
import consulo.language.psi.PsiElement;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 09.12.2015
 */
public class JavaScriptQuoteHandler extends SimpleTokenSetQuoteHandler implements JavaLikeQuoteHandler
{
	public JavaScriptQuoteHandler()
	{
		super(JavaScriptTokenSets.STRING_LITERALS);
	}

	@Override
	public TokenSet getConcatenatableStringTokenTypes()
	{
		return TokenSet.EMPTY;
	}

	@Override
	public String getStringConcatenationOperatorRepresentation()
	{
		return "";
	}

	@Override
	public TokenSet getStringTokenTypes()
	{
		return myLiteralTokenSet;
	}

	@Override
	public boolean isAppropriateElementTypeForLiteral(@Nonnull IElementType tokenType)
	{
		return JavaScriptTokenSets.COMMENTS.contains(tokenType) ||
				tokenType == JSTokenTypes.WHITE_SPACE ||
				tokenType == JSTokenTypes.SEMICOLON ||
				tokenType == JSTokenTypes.COMMA ||
				tokenType == JSTokenTypes.RPAR ||
				tokenType == JSTokenTypes.RBRACKET ||
				tokenType == JSTokenTypes.RBRACE ||
				tokenType == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL ||
				tokenType == JSTokenTypes.STRING_LITERAL;
	}

	@Override
	public boolean needParenthesesAroundConcatenation(PsiElement element)
	{
		return false;
	}
}
