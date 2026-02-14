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

import com.intellij.lang.javascript.JavaScriptHighlightingLexer;
import com.intellij.lang.javascript.*;
import consulo.colorScheme.TextAttributesKey;
import consulo.javascript.ide.hightlight.JavaScriptSyntaxHighlightKeys;
import consulo.language.ast.IElementType;
import consulo.language.ast.StringEscapesTokenTypes;
import consulo.language.editor.highlight.SyntaxHighlighterBase;
import consulo.language.lexer.Lexer;
import consulo.xml.editor.XmlHighlighterColors;
import jakarta.annotation.Nonnull;

import java.util.HashMap;
import java.util.Map;

/**
 * @author max
 * @since 2005-01-27
 */
@Deprecated
public class JSHighlighter extends SyntaxHighlighterBase implements JavaScriptSyntaxHighlightKeys {
    private static final Map<IElementType, TextAttributesKey> KEYS_1;
    private static final Map<IElementType, TextAttributesKey> KEYS_2;
    private final DialectOptionHolder myDialectOptionsHolder;

    public JSHighlighter(DialectOptionHolder dialectOptionsHolder) {
        myDialectOptionsHolder = dialectOptionsHolder;
    }

    @Nonnull
    @Override
    public Lexer getHighlightingLexer() {
        return new JavaScriptHighlightingLexer(myDialectOptionsHolder);
    }

    static {
        KEYS_1 = new HashMap<>();
        KEYS_2 = new HashMap<>();

        SyntaxHighlighterBase.fillMap(
            KEYS_1,
            JSTokenTypes.OPERATIONS,
            JS_OPERATION_SIGN
        ); // we do need init OPERATIONS before KEYWORDS because has some
        // OPERATIONS to be KEYWORDS (is, as)
        fillMap(KEYS_1, JSTokenTypes.KEYWORDS, JS_KEYWORD);

        KEYS_1.put(StringEscapesTokenTypes.VALID_STRING_ESCAPE_TOKEN, JS_VALID_STRING_ESCAPE);
        KEYS_1.put(StringEscapesTokenTypes.INVALID_CHARACTER_ESCAPE_TOKEN, JS_INVALID_STRING_ESCAPE);
        KEYS_1.put(StringEscapesTokenTypes.INVALID_UNICODE_ESCAPE_TOKEN, JS_INVALID_STRING_ESCAPE);

        KEYS_1.put(JSTokenTypes.NUMERIC_LITERAL, JS_NUMBER);
        KEYS_1.put(JSTokenTypes.STRING_LITERAL, JS_STRING);
        KEYS_1.put(JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL, JS_STRING);
        KEYS_1.put(JSTokenTypes.REGEXP_LITERAL, JS_REGEXP);

        KEYS_1.put(JSTokenTypes.LPAR, JS_PARENTHS);
        KEYS_1.put(JSTokenTypes.RPAR, JS_PARENTHS);

        KEYS_1.put(JSTokenTypes.LBRACE, JS_BRACES);
        KEYS_1.put(JSTokenTypes.RBRACE, JS_BRACES);

        KEYS_1.put(JSTokenTypes.LBRACKET, JS_BRACKETS);
        KEYS_1.put(JSTokenTypes.RBRACKET, JS_BRACKETS);

        KEYS_1.put(JSTokenTypes.COMMA, JS_COMMA);
        KEYS_1.put(JSTokenTypes.DOT, JS_DOT);
        KEYS_1.put(JSTokenTypes.SEMICOLON, JS_SEMICOLON);

        KEYS_1.put(JSTokenTypes.C_STYLE_COMMENT, JS_BLOCK_COMMENT);
        KEYS_1.put(JSTokenTypes.XML_STYLE_COMMENT, JS_BLOCK_COMMENT);
        KEYS_1.put(JSTokenTypes.DOC_COMMENT, JS_DOC_COMMENT);
        KEYS_1.put(JSTokenTypes.END_OF_LINE_COMMENT, JS_LINE_COMMENT);
        KEYS_1.put(JSTokenTypes.BAD_CHARACTER, JS_BAD_CHARACTER);

        KEYS_1.put(JSDocTokenTypes.DOC_TAG_NAME, JS_DOC_COMMENT);
        KEYS_2.put(JSDocTokenTypes.DOC_TAG_NAME, JS_DOC_TAG);

        IElementType[] javadoc = IElementType.enumerate(type -> type instanceof JSDocElementType);

        for (IElementType type : javadoc) {
            KEYS_1.put(type, JS_DOC_COMMENT);
        }

        KEYS_1.put(JSTokenTypes.JSDOC_TAG_DATA, JS_DOC_COMMENT);

        for (IElementType type : JSTokenTypes.XML_TOKENS.getTypes()) {
            KEYS_1.put(type, XmlHighlighterColors.XML_TAG);
        }

        KEYS_2.put(JSTokenTypes.XML_TAG_NAME, XmlHighlighterColors.XML_TAG_NAME);
        KEYS_2.put(JSTokenTypes.XML_NAME, XmlHighlighterColors.XML_ATTRIBUTE_NAME);
        KEYS_2.put(JSTokenTypes.XML_ATTR_VALUE, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
        KEYS_2.put(JSTokenTypes.XML_ATTR_VALUE_END, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
        KEYS_2.put(JSTokenTypes.XML_ATTR_VALUE_START, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
        KEYS_2.put(JSTokenTypes.XML_ATTR_EQUAL, XmlHighlighterColors.XML_ATTRIBUTE_VALUE);
        KEYS_2.put(JSTokenTypes.XML_ENTITY_REF, XmlHighlighterColors.XML_ENTITY_REFERENCE);

        KEYS_1.put(JSTokenTypes.XML_STYLE_COMMENT, XmlHighlighterColors.XML_COMMENT);
        KEYS_1.put(JSTokenTypes.XML_TAG_CONTENT, XmlHighlighterColors.XML_TAG_DATA);
    }

    @Nonnull
    @Override
    public TextAttributesKey[] getTokenHighlights(@Nonnull IElementType tokenType) {
        return pack(KEYS_1.get(tokenType), KEYS_2.get(tokenType));
    }
}
