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
import consulo.colorScheme.TextAttributesKey;
import consulo.language.ast.IElementType;
import consulo.language.ast.StringEscapesTokenTypes;
import consulo.language.ast.TokenSet;
import consulo.language.editor.highlight.SyntaxHighlighterBase;
import consulo.language.lexer.Lexer;
import consulo.util.collection.MultiMap;
import consulo.xml.editor.XmlHighlighterColors;

import javax.annotation.Nonnull;
import java.util.function.Supplier;

/**
 * @author VISTALL
 * @since 12.12.2015
 */
public class JavaScriptHighlighter extends SyntaxHighlighterBase
{
	public static void storeDefaults(MultiMap<IElementType, TextAttributesKey> keys)
	{
		TokenSet operations = TokenSet.andNot(JSTokenTypes.OPERATIONS, TokenSet.create(JSTokenTypes.AS_KEYWORD, JSTokenTypes.IS_KEYWORD));

		for(IElementType type : operations.getTypes())
		{
			keys.putValue(type, JavaScriptSyntaxHighlightKeys.JS_OPERATION_SIGN);
		}

		for(IElementType type : JSTokenTypes.KEYWORDS.getTypes())
		{
			keys.putValue(type, JavaScriptSyntaxHighlightKeys.JS_KEYWORD);
		}

		keys.putValue(StringEscapesTokenTypes.VALID_STRING_ESCAPE_TOKEN, JavaScriptSyntaxHighlightKeys.JS_VALID_STRING_ESCAPE);
		keys.putValue(StringEscapesTokenTypes.INVALID_CHARACTER_ESCAPE_TOKEN, JavaScriptSyntaxHighlightKeys.JS_INVALID_STRING_ESCAPE);
		keys.putValue(StringEscapesTokenTypes.INVALID_UNICODE_ESCAPE_TOKEN, JavaScriptSyntaxHighlightKeys.JS_INVALID_STRING_ESCAPE);

		keys.putValue(JSTokenTypes.NUMERIC_LITERAL, JavaScriptSyntaxHighlightKeys.JS_NUMBER);
		keys.putValue(JSTokenTypes.STRING_LITERAL, JavaScriptSyntaxHighlightKeys.JS_STRING);
		keys.putValue(JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL, JavaScriptSyntaxHighlightKeys.JS_STRING);
		keys.putValue(JSTokenTypes.INTERPOLATION_STRING_LITERAL, JavaScriptSyntaxHighlightKeys.JS_STRING);
		keys.putValue(JSTokenTypes.REGEXP_LITERAL, JavaScriptSyntaxHighlightKeys.JS_REGEXP);

		keys.putValue(JSTokenTypes.LPAR, JavaScriptSyntaxHighlightKeys.JS_PARENTHS);
		keys.putValue(JSTokenTypes.RPAR, JavaScriptSyntaxHighlightKeys.JS_PARENTHS);

		keys.putValue(JSTokenTypes.LBRACE, JavaScriptSyntaxHighlightKeys.JS_BRACES);
		keys.putValue(JSTokenTypes.RBRACE, JavaScriptSyntaxHighlightKeys.JS_BRACES);

		keys.putValue(JSTokenTypes.LBRACKET, JavaScriptSyntaxHighlightKeys.JS_BRACKETS);
		keys.putValue(JSTokenTypes.RBRACKET, JavaScriptSyntaxHighlightKeys.JS_BRACKETS);

		keys.putValue(JSTokenTypes.COMMA, JavaScriptSyntaxHighlightKeys.JS_COMMA);
		keys.putValue(JSTokenTypes.DOT, JavaScriptSyntaxHighlightKeys.JS_DOT);
		keys.putValue(JSTokenTypes.SEMICOLON, JavaScriptSyntaxHighlightKeys.JS_SEMICOLON);

		keys.putValue(JSTokenTypes.C_STYLE_COMMENT, JavaScriptSyntaxHighlightKeys.JS_BLOCK_COMMENT);
		keys.putValue(JSTokenTypes.XML_STYLE_COMMENT, JavaScriptSyntaxHighlightKeys.JS_BLOCK_COMMENT);
		keys.putValue(JSTokenTypes.DOC_COMMENT, JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT);
		keys.putValue(JSTokenTypes.END_OF_LINE_COMMENT, JavaScriptSyntaxHighlightKeys.JS_LINE_COMMENT);
		keys.putValue(JSTokenTypes.BAD_CHARACTER, JavaScriptSyntaxHighlightKeys.JS_BAD_CHARACTER);

		keys.putValue(JSDocTokenTypes.DOC_TAG_NAME, JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT);
		keys.putValue(JSDocTokenTypes.DOC_TAG_NAME, JavaScriptSyntaxHighlightKeys.JS_DOC_TAG);

		IElementType[] javadoc = IElementType.enumerate(type -> type instanceof JSDocElementType);

		for(IElementType type : javadoc)
		{
			keys.putValue(type, JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT);
		}

		keys.putValue(JSTokenTypes.JSDOC_TAG_DATA, JavaScriptSyntaxHighlightKeys.JS_DOC_COMMENT);

		for(IElementType type : JSTokenTypes.XML_TOKENS.getTypes())
		{
			keys.putValue(type, XmlHighlighterColors.XML_TAG);
		}

		keys.putValue(JSTokenTypes.XML_TAG_NAME, XmlHighlighterColors.XML_TAG_NAME);
		keys.putValue(JSTokenTypes.XML_NAME, XmlHighlighterColors.XML_ATTRIBUTE_NAME);
		keys.putValue(JSTokenTypes.XML_ATTR_VALUE, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
		keys.putValue(JSTokenTypes.XML_ATTR_VALUE_END, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
		keys.putValue(JSTokenTypes.XML_ATTR_VALUE_START, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
		keys.putValue(JSTokenTypes.XML_ATTR_EQUAL, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
		keys.putValue(JSTokenTypes.XML_ENTITY_REF, XmlHighlighterColors.XML_ENTITY_REFERENCE);

		keys.putValue(JSTokenTypes.XML_STYLE_COMMENT, XmlHighlighterColors.XML_COMMENT);
		keys.putValue(JSTokenTypes.XML_TAG_CONTENT, XmlHighlighterColors.XML_TAG_DATA);
	}

	private final Supplier<Lexer> myFactory;
	private final MultiMap<IElementType, TextAttributesKey> keys = MultiMap.createLinked();

	public JavaScriptHighlighter(@Nonnull Supplier<Lexer> factory)
	{
		myFactory = factory;

		storeDefaults(keys);
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
		return keys.get(tokenType).toArray(new TextAttributesKey[0]);
	}
}
