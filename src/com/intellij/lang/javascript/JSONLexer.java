/*
 * @author max
 */
package com.intellij.lang.javascript;

import org.jetbrains.annotations.Nullable;
import com.intellij.lexer.DelegateLexer;
import com.intellij.lexer.Lexer;
import com.intellij.psi.StringEscapesTokenTypes;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;

public final class JSONLexer extends DelegateLexer
{
	private static final TokenSet purAllowedTokenSet = TokenSet.orSet(JSTokenTypes.IDENTIFIER_TOKENS_SET, TokenSet.create(JSTokenTypes.NULL_KEYWORD,
			JSTokenTypes.LBRACE, JSTokenTypes.LBRACKET, JSTokenTypes.RBRACE, JSTokenTypes.RBRACKET, JSTokenTypes.TRUE_KEYWORD, JSTokenTypes.C_STYLE_COMMENT,
			JSTokenTypes.END_OF_LINE_COMMENT, JSTokenTypes.FALSE_KEYWORD, JSTokenTypes.NUMERIC_LITERAL, JSTokenTypes.STRING_LITERAL,
			JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL, JSTokenTypes.COMMA, JSTokenTypes.COLON, JSTokenTypes.WHITE_SPACE, JSTokenTypes.MINUS,
			StringEscapesTokenTypes.INVALID_CHARACTER_ESCAPE_TOKEN, StringEscapesTokenTypes.VALID_STRING_ESCAPE_TOKEN,
			StringEscapesTokenTypes.INVALID_UNICODE_ESCAPE_TOKEN));

	public JSONLexer(Lexer baseLexer)
	{
		super(baseLexer);
	}

	@Override
	@Nullable
	public IElementType getTokenType()
	{
		final IElementType tokenType = super.getTokenType();
		if(purAllowedTokenSet.contains(tokenType) || tokenType == null)
		{
			return tokenType;
		}
		return JSTokenTypes.BAD_CHARACTER;
	}
}