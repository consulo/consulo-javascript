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

import consulo.codeEditor.DefaultLanguageHighlighterColors;
import consulo.colorScheme.TextAttributes;
import consulo.colorScheme.TextAttributesKey;
import consulo.codeEditor.HighlighterColors;
import consulo.ui.color.RGBColor;
import consulo.ui.style.StandardColors;

/**
 * @author VISTALL
 * @since 12.12.2015
 */
public interface JavaScriptSyntaxHighlightKeys {
    TextAttributesKey JS_KEYWORD = TextAttributesKey.of("JS.KEYWORD", DefaultLanguageHighlighterColors.KEYWORD);

    TextAttributesKey JS_STRING = TextAttributesKey.of("JS.STRING", DefaultLanguageHighlighterColors.STRING);

    TextAttributesKey JS_NUMBER = TextAttributesKey.of("JS.NUMBER", DefaultLanguageHighlighterColors.NUMBER);

    TextAttributesKey JS_REGEXP = TextAttributesKey.of("JS.REGEXP", DefaultLanguageHighlighterColors.NUMBER);

    TextAttributesKey JS_LINE_COMMENT = TextAttributesKey.of("JS.LINE_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);

    TextAttributesKey JS_BLOCK_COMMENT =
        TextAttributesKey.of("JS.BLOCK_COMMENT", DefaultLanguageHighlighterColors.BLOCK_COMMENT);

    TextAttributesKey JS_DOC_COMMENT = TextAttributesKey.of("JS.DOC_COMMENT", DefaultLanguageHighlighterColors.DOC_COMMENT);

    TextAttributesKey JS_OPERATION_SIGN =
        TextAttributesKey.of("JS.OPERATION_SIGN", DefaultLanguageHighlighterColors.OPERATION_SIGN);

    TextAttributesKey JS_PARENTHS = TextAttributesKey.of("JS.PARENTHS", DefaultLanguageHighlighterColors.PARENTHESES);

    TextAttributesKey JS_BRACKETS = TextAttributesKey.of("JS.BRACKETS", DefaultLanguageHighlighterColors.BRACKETS);

    TextAttributesKey JS_BRACES = TextAttributesKey.of("JS.BRACES", DefaultLanguageHighlighterColors.BRACES);

    TextAttributesKey JS_COMMA = TextAttributesKey.of("JS.COMMA", DefaultLanguageHighlighterColors.COMMA);

    TextAttributesKey JS_DOT = TextAttributesKey.of("JS.DOT", DefaultLanguageHighlighterColors.DOT);

    TextAttributesKey JS_SEMICOLON = TextAttributesKey.of("JS.SEMICOLON", DefaultLanguageHighlighterColors.SEMICOLON);

    TextAttributesKey JS_BAD_CHARACTER = TextAttributesKey.of("JS.BADCHARACTER", HighlighterColors.BAD_CHARACTER);
    TextAttributesKey JS_DOC_TAG = TextAttributesKey.of("JS.DOC_TAG", DefaultLanguageHighlighterColors.DOC_COMMENT_TAG);
    TextAttributesKey JS_DOC_MARKUP = TextAttributesKey.of("JS.DOC_MARKUP", DefaultLanguageHighlighterColors.DOC_COMMENT_MARKUP);
    TextAttributesKey JS_VALID_STRING_ESCAPE =
        TextAttributesKey.of("JS.VALID_STRING_ESCAPE", DefaultLanguageHighlighterColors.VALID_STRING_ESCAPE);
    TextAttributesKey JS_INVALID_STRING_ESCAPE =
        TextAttributesKey.of("JS.INVALID_STRING_ESCAPE", DefaultLanguageHighlighterColors.INVALID_STRING_ESCAPE);
    TextAttributesKey JS_LOCAL_VARIABLE =
        TextAttributesKey.of("JS.LOCAL_VARIABLE", DefaultLanguageHighlighterColors.LOCAL_VARIABLE);
    TextAttributesKey JS_PARAMETER = TextAttributesKey.of("JS.PARAMETER", DefaultLanguageHighlighterColors.PARAMETER);
    TextAttributesKey JS_INSTANCE_MEMBER_VARIABLE =
        TextAttributesKey.of("JS.INSTANCE_MEMBER_VARIABLE", DefaultLanguageHighlighterColors.INSTANCE_FIELD);
    TextAttributesKey JS_STATIC_MEMBER_VARIABLE =
        TextAttributesKey.of("JS.STATIC_MEMBER_VARIABLE", DefaultLanguageHighlighterColors.STATIC_FIELD);
    TextAttributesKey JS_GLOBAL_VARIABLE =
        TextAttributesKey.of("JS.GLOBAL_VARIABLE", DefaultLanguageHighlighterColors.STATIC_FIELD);
    TextAttributesKey JS_GLOBAL_FUNCTION =
        TextAttributesKey.of("JS.GLOBAL_FUNCTION", DefaultLanguageHighlighterColors.STATIC_METHOD);
    TextAttributesKey JS_STATIC_MEMBER_FUNCTION =
        TextAttributesKey.of("JS.STATIC_MEMBER_FUNCTION", DefaultLanguageHighlighterColors.STATIC_METHOD);
    TextAttributesKey JS_INSTANCE_MEMBER_FUNCTION = TextAttributesKey.createTextAttributesKey(
        "JS.INSTANCE_MEMBER_FUNCTION",
        new TextAttributes(new RGBColor(0x7a, 0x7a, 43), StandardColors.WHITE, null, null, 0)
    );

    TextAttributesKey JS_METADATA = TextAttributesKey.createTextAttributesKey(
        "JS.ATTRIBUTE",
        new TextAttributes(null, new RGBColor(0xf7, 0xe9, 0xe9), null, null, 0)
    );
}
