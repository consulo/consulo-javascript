/*
 * Copyright 2000-2005 JetBrains s.r.o
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

package com.intellij.lang.javascript.highlighting;

import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript._JSDocLexer;
import consulo.language.ast.IElementType;
import consulo.language.editor.highlight.SyntaxHighlighterFactory;
import consulo.language.lexer.*;
import consulo.xml.lang.html.HTMLLanguage;

import java.util.function.Supplier;

/**
 * @author max
 * @since 12:13:05 AM Feb 15, 2005
 */
public class JavaScriptHighlightingLexer extends LayeredLexer
{
	public JavaScriptHighlightingLexer(Supplier<Lexer> baseLexerFactory)
	{
		this(baseLexerFactory, true);
	}

	private JavaScriptHighlightingLexer(Supplier<Lexer> baseLexerFactory, boolean withEmbeddments)
	{
		super(baseLexerFactory.get());

		if(withEmbeddments)
		{
			registerSelfStoppingLayer(new StringLiteralLexer('\"', JSTokenTypes.STRING_LITERAL, true, "/"), new IElementType[]{JSTokenTypes.STRING_LITERAL},
					IElementType.EMPTY_ARRAY);

			registerSelfStoppingLayer(new JavaScriptHighlightingLexer(baseLexerFactory, false), new IElementType[]{JSTokenTypes.XML_JS_SCRIPT},
					IElementType.EMPTY_ARRAY);

			registerSelfStoppingLayer(new StringLiteralLexer('\'', JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL, true, "/"),
					new IElementType[]{JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL}, IElementType.EMPTY_ARRAY);

			final LayeredLexer docLexer = new LayeredLexer(new FlexAdapter(new _JSDocLexer(true)));
			final Lexer lexer = getHtmlHighlightingLexer();

			// Force html highlighting lexer not to return tag content type since it causes token type collision in javascript embedded to html
			docLexer.registerLayer(new LexerBase()
			{
				final IElementType tagContentType = getTagContentTokenType();


				@Override
				public void start(final CharSequence buffer, final int startOffset, final int endOffset, final int initialState)
				{
					lexer.start(buffer, startOffset, endOffset, initialState);
				}

				@Override
				public int getState()
				{
					return lexer.getState();
				}

				@Override
				public IElementType getTokenType()
				{
					final IElementType tokenType = lexer.getTokenType();
					if(tokenType == tagContentType)
					{
						return JSTokenTypes.JSDOC_TAG_DATA;
					}
					return tokenType;
				}

				@Override
				public int getTokenStart()
				{
					return lexer.getTokenStart();
				}

				@Override
				public int getTokenEnd()
				{
					return lexer.getTokenEnd();
				}

				@Override
				public void advance()
				{
					lexer.advance();
				}

				@Override
				public CharSequence getBufferSequence()
				{
					return lexer.getBufferSequence();
				}


				@Override
				public int getBufferEnd()
				{
					return lexer.getBufferEnd();
				}
			}, JSDocTokenTypes.DOC_COMMENT_DATA);
			registerSelfStoppingLayer(docLexer, new IElementType[]{JSTokenTypes.DOC_COMMENT}, new IElementType[]{JSDocTokenTypes.DOC_COMMENT_END});
		}
	}

	public static IElementType getTagContentTokenType()
	{
		final Lexer highlightingLexer = getHtmlHighlightingLexer();
		highlightingLexer.start("a", 0, 1, 0);
		return highlightingLexer.getTokenType();
	}

	private static Lexer getHtmlHighlightingLexer()
	{
		return SyntaxHighlighterFactory.getSyntaxHighlighter(HTMLLanguage.INSTANCE, null, null).getHighlightingLexer();
	}
}
