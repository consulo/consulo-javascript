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

import java.util.HashMap;
import java.util.Map;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.javascript.ide.hightlight.JavaScriptSyntaxHighlightKeys;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JSDocElementType;
import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptHighlightingLexer;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.XmlHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.StringEscapesTokenTypes;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.Processor;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 27, 2005
 * Time: 11:22:04 PM
 */
@Deprecated
public class JSHighlighter extends SyntaxHighlighterBase implements JavaScriptSyntaxHighlightKeys
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
}
