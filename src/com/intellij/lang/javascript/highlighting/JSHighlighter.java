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

package com.intellij.lang.javascript.highlighting;

import java.awt.Color;
import java.awt.Font;
import java.util.HashMap;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.daemon.impl.HighlightInfoType;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JSDocElementType;
import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptHighlightingLexer;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.HighlighterColors;
import com.intellij.openapi.editor.XmlHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.EffectType;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.StringEscapesTokenTypes;
import com.intellij.psi.tree.IElementType;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 27, 2005
 * Time: 11:22:04 PM
 */
public class JSHighlighter extends SyntaxHighlighterBase
{
	private static Map<IElementType, TextAttributesKey> keys1;
	private static Map<IElementType, TextAttributesKey> keys2;
	private final DialectOptionHolder myDialectOptionsHolder;

	public JSHighlighter(DialectOptionHolder dialectOptionsHolder)
	{
		myDialectOptionsHolder = dialectOptionsHolder;
	}

	@Override
	@NotNull
	public Lexer getHighlightingLexer()
	{
		return new JavaScriptHighlightingLexer(myDialectOptionsHolder);
	}

	static final TextAttributesKey JS_KEYWORD = TextAttributesKey.createTextAttributesKey("JS.KEYWORD", DefaultLanguageHighlighterColors.KEYWORD);

	static final TextAttributesKey JS_STRING = TextAttributesKey.createTextAttributesKey("JS.STRING", DefaultLanguageHighlighterColors.STRING);

	static final TextAttributesKey JS_NUMBER = TextAttributesKey.createTextAttributesKey("JS.NUMBER", DefaultLanguageHighlighterColors.NUMBER);

	static final TextAttributesKey JS_REGEXP = TextAttributesKey.createTextAttributesKey("JS.REGEXP", new TextAttributes(Color.blue.brighter(), null,
			null, null, Font.PLAIN));

	static final TextAttributesKey JS_LINE_COMMENT = TextAttributesKey.createTextAttributesKey("JS.LINE_COMMENT",
			DefaultLanguageHighlighterColors.LINE_COMMENT);

	static final TextAttributesKey JS_BLOCK_COMMENT = TextAttributesKey.createTextAttributesKey("JS.BLOCK_COMMENT",
			DefaultLanguageHighlighterColors.BLOCK_COMMENT);

	static final TextAttributesKey JS_DOC_COMMENT = TextAttributesKey.createTextAttributesKey("JS.DOC_COMMENT",
			DefaultLanguageHighlighterColors.DOC_COMMENT);

	public static final TextAttributesKey JS_OPERATION_SIGN = TextAttributesKey.createTextAttributesKey("JS.OPERATION_SIGN",
			DefaultLanguageHighlighterColors.OPERATION_SIGN);

	static final TextAttributesKey JS_PARENTHS = TextAttributesKey.createTextAttributesKey("JS.PARENTHS", DefaultLanguageHighlighterColors.PARENTHESES);

	static final TextAttributesKey JS_BRACKETS = TextAttributesKey.createTextAttributesKey("JS.BRACKETS", DefaultLanguageHighlighterColors.BRACKETS);

	static final TextAttributesKey JS_BRACES = TextAttributesKey.createTextAttributesKey("JS.BRACES", DefaultLanguageHighlighterColors.BRACES);

	static final TextAttributesKey JS_COMMA = TextAttributesKey.createTextAttributesKey("JS.COMMA", DefaultLanguageHighlighterColors.COMMA);

	static final TextAttributesKey JS_DOT = TextAttributesKey.createTextAttributesKey("JS.DOT", DefaultLanguageHighlighterColors.DOT);

	static final TextAttributesKey JS_SEMICOLON = TextAttributesKey.createTextAttributesKey("JS.SEMICOLON", DefaultLanguageHighlighterColors.SEMICOLON);

	static final TextAttributesKey JS_BAD_CHARACTER = TextAttributesKey.createTextAttributesKey("JS.BADCHARACTER", HighlighterColors.BAD_CHARACTER);
	static final TextAttributesKey JS_DOC_TAG = TextAttributesKey.createTextAttributesKey("JS.DOC_TAG",
			DefaultLanguageHighlighterColors.DOC_COMMENT_TAG);
	static final TextAttributesKey JS_DOC_MARKUP = TextAttributesKey.createTextAttributesKey("JS.DOC_MARKUP",
			DefaultLanguageHighlighterColors.DOC_COMMENT_MARKUP);
	static final TextAttributesKey JS_VALID_STRING_ESCAPE = TextAttributesKey.createTextAttributesKey("JS.VALID_STRING_ESCAPE",
			DefaultLanguageHighlighterColors.VALID_STRING_ESCAPE);
	static final TextAttributesKey JS_INVALID_STRING_ESCAPE = TextAttributesKey.createTextAttributesKey("JS.INVALID_STRING_ESCAPE",
			DefaultLanguageHighlighterColors.INVALID_STRING_ESCAPE);
	static final TextAttributesKey JS_LOCAL_VARIABLE = TextAttributesKey.createTextAttributesKey("JS.LOCAL_VARIABLE", new TextAttributes(new Color(69,
			131, 131), Color.white, null, null, 0));
	static final TextAttributesKey JS_PARAMETER = TextAttributesKey.createTextAttributesKey("JS.PARAMETER", new TextAttributes(Color.black,
			Color.white, Color.black, EffectType.LINE_UNDERSCORE, 0));
	static final TextAttributesKey JS_INSTANCE_MEMBER_VARIABLE = TextAttributesKey.createTextAttributesKey("JS.INSTANCE_MEMBER_VARIABLE",
			HighlightInfoType.INSTANCE_FIELD.getAttributesKey());
	static final TextAttributesKey JS_STATIC_MEMBER_VARIABLE = TextAttributesKey.createTextAttributesKey("JS.STATIC_MEMBER_VARIABLE",
			HighlightInfoType.STATIC_FIELD.getAttributesKey());
	static final TextAttributesKey JS_GLOBAL_VARIABLE = TextAttributesKey.createTextAttributesKey("JS.GLOBAL_VARIABLE",
			HighlightInfoType.STATIC_FIELD.getAttributesKey());
	static final TextAttributesKey JS_GLOBAL_FUNCTION = TextAttributesKey.createTextAttributesKey("JS.GLOBAL_FUNCTION",
			HighlightInfoType.STATIC_METHOD.getAttributesKey());
	static final TextAttributesKey JS_STATIC_MEMBER_FUNCTION = TextAttributesKey.createTextAttributesKey("JS.STATIC_MEMBER_FUNCTION",
			HighlightInfoType.STATIC_METHOD.getAttributesKey());
	static final TextAttributesKey JS_INSTANCE_MEMBER_FUNCTION = TextAttributesKey.createTextAttributesKey("JS.INSTANCE_MEMBER_FUNCTION",
			new TextAttributes(new Color(0x7a, 0x7a, 43), Color.white, null, null, 0));
	static final TextAttributesKey JS_METADATA = TextAttributesKey.createTextAttributesKey("JS.ATTRIBUTE", new TextAttributes(null, new Color(0xf7,
			0xe9, 0xe9), null, null, 0));

	static
	{
		keys1 = new HashMap<IElementType, TextAttributesKey>();
		keys2 = new HashMap<IElementType, TextAttributesKey>();

		SyntaxHighlighterBase.fillMap(keys1, JSTokenTypes.OPERATIONS, JS_OPERATION_SIGN); // we do need init OPERATIONS before KEYWORDS because has some
		// OPERATIONS to be KEYWORDS (is, as)
		fillMap(keys1, JSTokenTypes.KEYWORDS, JS_KEYWORD);

		keys1.put(StringEscapesTokenTypes.VALID_STRING_ESCAPE_TOKEN, JS_VALID_STRING_ESCAPE);
		keys1.put(StringEscapesTokenTypes.INVALID_CHARACTER_ESCAPE_TOKEN, JS_INVALID_STRING_ESCAPE);
		keys1.put(StringEscapesTokenTypes.INVALID_UNICODE_ESCAPE_TOKEN, JS_INVALID_STRING_ESCAPE);

		keys1.put(JSTokenTypes.NUMERIC_LITERAL, JS_NUMBER);
		keys1.put(JSTokenTypes.STRING_LITERAL, JS_STRING);
		keys1.put(JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL, JS_STRING);
		keys1.put(JSTokenTypes.REGEXP_LITERAL, JS_REGEXP);

		keys1.put(JSTokenTypes.LPAR, JS_PARENTHS);
		keys1.put(JSTokenTypes.RPAR, JS_PARENTHS);

		keys1.put(JSTokenTypes.LBRACE, JS_BRACES);
		keys1.put(JSTokenTypes.RBRACE, JS_BRACES);

		keys1.put(JSTokenTypes.LBRACKET, JS_BRACKETS);
		keys1.put(JSTokenTypes.RBRACKET, JS_BRACKETS);

		keys1.put(JSTokenTypes.COMMA, JS_COMMA);
		keys1.put(JSTokenTypes.DOT, JS_DOT);
		keys1.put(JSTokenTypes.SEMICOLON, JS_SEMICOLON);

		keys1.put(JSTokenTypes.C_STYLE_COMMENT, JS_BLOCK_COMMENT);
		keys1.put(JSTokenTypes.XML_STYLE_COMMENT, JS_BLOCK_COMMENT);
		keys1.put(JSTokenTypes.DOC_COMMENT, JS_DOC_COMMENT);
		keys1.put(JSTokenTypes.END_OF_LINE_COMMENT, JS_LINE_COMMENT);
		keys1.put(JSTokenTypes.BAD_CHARACTER, JS_BAD_CHARACTER);

		keys1.put(JSDocTokenTypes.DOC_TAG_NAME, JS_DOC_COMMENT);
		keys2.put(JSDocTokenTypes.DOC_TAG_NAME, JS_DOC_TAG);

		IElementType[] javadoc = IElementType.enumerate(new IElementType.Predicate()
		{
			@Override
			public boolean matches(IElementType type)
			{
				return type instanceof JSDocElementType;
			}
		});

		for(IElementType type : javadoc)
		{
			keys1.put(type, JS_DOC_COMMENT);
		}

		keys1.put(JSTokenTypes.JSDOC_TAG_DATA, JS_DOC_COMMENT);

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

	@Override
	@NotNull
	public TextAttributesKey[] getTokenHighlights(IElementType tokenType)
	{
		return pack(keys1.get(tokenType), keys2.get(tokenType));
	}

	public Map<IElementType, TextAttributesKey> getKeys1()
	{
		return (Map<IElementType, TextAttributesKey>) ((HashMap) keys1).clone();
	}

	public Map<IElementType, TextAttributesKey> getKeys2()
	{
		return (Map<IElementType, TextAttributesKey>) ((HashMap) keys2).clone();
	}

	public static void registerHtmlMarkup(IElementType[] htmlTokens, IElementType[] htmlTokens2)
	{
		for(IElementType idx : htmlTokens)
		{
			keys1.put(idx, JS_DOC_COMMENT);
			keys2.put(idx, JS_DOC_MARKUP);
		}

		for(IElementType idx : htmlTokens2)
		{
			keys1.put(idx, JS_DOC_COMMENT);
		}
	}
}
