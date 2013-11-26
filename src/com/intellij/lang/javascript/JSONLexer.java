/*
 * @author max
 */
package com.intellij.lang.javascript;

import org.jetbrains.annotations.Nullable;
import com.intellij.lexer.Lexer;
import com.intellij.lexer.LexerBase;
import com.intellij.psi.StringEscapesTokenTypes;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;

public final class JSONLexer extends LexerBase
{
	private final Lexer myLexer;
	private final TokenSet myAllowedTokenSet = TokenSet.orSet(JSTokenTypes.IDENTIFIER_TOKENS_SET, TokenSet.create(JSTokenTypes.NULL_KEYWORD,
			JSTokenTypes.LBRACE, JSTokenTypes.LBRACKET, JSTokenTypes.RBRACE, JSTokenTypes.RBRACKET, JSTokenTypes.TRUE_KEYWORD,
			JSTokenTypes.C_STYLE_COMMENT, JSTokenTypes.END_OF_LINE_COMMENT, JSTokenTypes.FALSE_KEYWORD, JSTokenTypes.NUMERIC_LITERAL,
			JSTokenTypes.STRING_LITERAL, JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL, JSTokenTypes.COMMA, JSTokenTypes.COLON, JSTokenTypes.WHITE_SPACE,
			JSTokenTypes.MINUS, StringEscapesTokenTypes.INVALID_CHARACTER_ESCAPE_TOKEN, StringEscapesTokenTypes.VALID_STRING_ESCAPE_TOKEN,
			StringEscapesTokenTypes.INVALID_UNICODE_ESCAPE_TOKEN));

	public JSONLexer(Lexer baseLexer)
	{
		myLexer = baseLexer;
	}

	public void start(final CharSequence buffer, final int startOffset, final int endOffset, final int initialState)
	{
		myLexer.start(buffer, startOffset, endOffset, initialState);
	}

	public int getState()
	{
		return myLexer.getState();
	}

	@Nullable
	public IElementType getTokenType()
	{
		final IElementType tokenType = myLexer.getTokenType();
		if(myAllowedTokenSet.contains(tokenType) || tokenType == null)
		{
			return tokenType;
		}
		return JSTokenTypes.BAD_CHARACTER;
	}

	public int getTokenStart()
	{
		return myLexer.getTokenStart();
	}

	public int getTokenEnd()
	{
		return myLexer.getTokenEnd();
	}

	public void advance()
	{
		myLexer.advance();
	}

	@Override
	public CharSequence getBufferSequence()
	{
		return myLexer.getBufferSequence();
	}

	public int getBufferEnd()
	{
		return myLexer.getBufferEnd();
	}
}