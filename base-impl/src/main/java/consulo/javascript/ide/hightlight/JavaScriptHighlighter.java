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

package consulo.javascript.ide.hightlight;

import com.intellij.lang.javascript.JSDocElementType;
import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.highlighting.JavaScriptHighlightingLexer;
import consulo.application.util.function.Processor;
import consulo.colorScheme.TextAttributesKey;
import consulo.language.ast.IElementType;
import consulo.language.ast.StringEscapesTokenTypes;
import consulo.language.ast.TokenSet;
import consulo.language.editor.highlight.SyntaxHighlighterBase;
import consulo.language.impl.ast.Factory;
import consulo.language.lexer.Lexer;
import consulo.xml.editor.XmlHighlighterColors;

import javax.annotation.Nonnull;
import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

/**
 * @author VISTALL
 * @since 12.12.2015
 */
public class JavaScriptHighlighter extends SyntaxHighlighterBase
{
	private static Map<IElementType, TextAttributesKey> keys1;
	private static Map<IElementType, TextAttributesKey> keys2;

	static
	{
		keys1 = new HashMap<IElementType, TextAttributesKey>();
		keys2 = new HashMap<IElementType, TextAttributesKey>();

		TokenSet operations = TokenSet.andNot(JSTokenTypes.OPERATIONS, TokenSet.create(JSTokenTypes.AS_KEYWORD, JSTokenTypes.IS_KEYWORD));

		SyntaxHighlighterBase.safeMap(keys1, operations, JavaScriptSyntaxHighlightKeys.JS_OPERATION_SIGN);

		safeMap(keys1, JSTokenTypes.KEYWORDS, JavaScriptSyntaxHighlightKeys.JS_KEYWORD);

		keys1.put(StringEscapesTokenTypes.VALID_STRING_ESCAPE_TOKEN, JavaScriptSyntaxHighlightKeys.JS_VALID_STRING_ESCAPE);
		keys1.put(StringEscapesTokenTypes.INVALID_CHARACTER_ESCAPE_TOKEN, JavaScriptSyntaxHighlightKeys.JS_INVALID_STRING_ESCAPE);
		keys1.put(StringEscapesTokenTypes.INVALID_UNICODE_ESCAPE_TOKEN, JavaScriptSyntaxHighlightKeys.JS_INVALID_STRING_ESCAPE);

		keys1.put(JSTokenTypes.NUMERIC_LITERAL, JavaScriptSyntaxHighlightKeys.JS_NUMBER);
		keys1.put(JSTokenTypes.STRING_LITERAL, JavaScriptSyntaxHighlightKeys.JS_STRING);
		keys1.put(JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL, JavaScriptSyntaxHighlightKeys.JS_STRING);
		keys1.put(JSTokenTypes.INTERPOLATION_STRING_LITERAL, JavaScriptSyntaxHighlightKeys.JS_STRING);
		keys1.put(JSTokenTypes.REGEXP_LITERAL, JavaScriptSyntaxHighlightKeys.JS_REGEXP);

		keys1.put(JSTokenTypes.LPAR, JavaScriptSyntaxHighlightKeys.JS_PARENTHS);
		keys1.put(JSTokenTypes.RPAR, JavaScriptSyntaxHighlightKeys.JS_PARENTHS);

		keys1.put(JSTokenTypes.LBRACE, JavaScriptSyntaxHighlightKeys.JS_BRACES);
		keys1.put(JSTokenTypes.RBRACE, JavaScriptSyntaxHighlightKeys.JS_BRACES);

		keys1.put(JSTokenTypes.LBRACKET, JavaScriptSyntaxHighlightKeys.JS_BRACKETS);
		keys1.put(JSTokenTypes.RBRACKET, JavaScriptSyntaxHighlightKeys.JS_BRACKETS);

		keys1.put(JSTokenTypes.COMMA, JavaScriptSyntaxHighlightKeys.JS_COMMA);
		keys1.put(JSTokenTypes.DOT, JavaScriptSyntaxHighlightKeys.JS_DOT);
		keys1.put(JSTokenTypes.SEMICOLON, JavaScriptSyntaxHighlightKeys.JS_SEMICOLON);

		keys1.put(JSTokenTypes.C_STYLE_COMMENT, JavaScriptSyntaxHighlightKeys.JS_BLOCK_COMMENT);
		keys1.put(JSTokenTypes.XML_STYLE_COMMENT, JavaScriptSyntaxHighlightKeys.JS_BLOCK_COMMENT);
		keys1.put(JSTokenTypes.DOC_COMMENT, JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT);
		keys1.put(JSTokenTypes.END_OF_LINE_COMMENT, JavaScriptSyntaxHighlightKeys.JS_LINE_COMMENT);
		keys1.put(JSTokenTypes.BAD_CHARACTER, JavaScriptSyntaxHighlightKeys.JS_BAD_CHARACTER);

		keys1.put(JSDocTokenTypes.DOC_TAG_NAME, JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT);
		keys2.put(JSDocTokenTypes.DOC_TAG_NAME, JavaScriptSyntaxHighlightKeys.JS_DOC_TAG);

		IElementType[] javadoc = IElementType.enumerate(new Processor<IElementType>()
		{
			@Override
			public boolean process(IElementType type)
			{
				return type instanceof JSDocElementType;
			}
		});

		for(IElementType type : javadoc)
		{
			keys1.put(type, JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT);
		}

		keys1.put(JSTokenTypes.JSDOC_TAG_DATA, JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT);

		for(IElementType type : JSTokenTypes.XML_TOKENS.getTypes())
		{
			keys1.put(type, XmlHighlighterColors.XML_TAG);
		}

		keys2.put(JSTokenTypes.XML_TAG_NAME, XmlHighlighterColors.XML_TAG_NAME);
		keys2.put(JSTokenTypes.XML_NAME, XmlHighlighterColors.XML_ATTRIBUTE_NAME);
		keys2.put(JSTokenTypes.XML_ATTR_VALUE, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
		keys2.put(JSTokenTypes.XML_ATTR_VALUE_END, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
		keys2.put(JSTokenTypes.XML_ATTR_VALUE_START, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
		keys2.put(JSTokenTypes.XML_ATTR_EQUAL, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
		keys2.put(JSTokenTypes.XML_ENTITY_REF, XmlHighlighterColors.XML_ENTITY_REFERENCE);

		keys1.put(JSTokenTypes.XML_STYLE_COMMENT, XmlHighlighterColors.XML_COMMENT);
		keys1.put(JSTokenTypes.XML_TAG_CONTENT, XmlHighlighterColors.XML_TAG_DATA);
	}

	private Supplier<Lexer> myFactory;

	public JavaScriptHighlighter(@Nonnull Supplier<Lexer> factory)
	{
		myFactory = factory;
	}

	@Nonnull
	@Override
	public Lexer getHighlightingLexer()
	{
		return new JavaScriptHighlightingLexer(myFactory);
	}

	@Override
	@Nonnull
	public TextAttributesKey[] getTokenHighlights(IElementType tokenType)
	{
		return pack(keys1.get(tokenType), keys2.get(tokenType));
	}
}
