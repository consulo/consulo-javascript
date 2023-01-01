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
public interface JavaScriptSyntaxHighlightKeys
{
	TextAttributesKey JS_KEYWORD = TextAttributesKey.createTextAttributesKey("JS.KEYWORD", DefaultLanguageHighlighterColors.KEYWORD);

	TextAttributesKey JS_STRING = TextAttributesKey.createTextAttributesKey("JS.STRING", DefaultLanguageHighlighterColors.STRING);

	TextAttributesKey JS_NUMBER = TextAttributesKey.createTextAttributesKey("JS.NUMBER", DefaultLanguageHighlighterColors.NUMBER);

	TextAttributesKey JS_REGEXP = TextAttributesKey.createTextAttributesKey("JS.REGEXP", DefaultLanguageHighlighterColors.NUMBER);

	TextAttributesKey JS_LINE_COMMENT = TextAttributesKey.createTextAttributesKey("JS.LINE_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);

	TextAttributesKey JS_BLOCK_COMMENT = TextAttributesKey.createTextAttributesKey("JS.BLOCK_COMMENT", DefaultLanguageHighlighterColors.BLOCK_COMMENT);

	TextAttributesKey JS_DOC_COMMENT = TextAttributesKey.createTextAttributesKey("JS.DOC_COMMENT", DefaultLanguageHighlighterColors.DOC_COMMENT);

	TextAttributesKey JS_OPERATION_SIGN = TextAttributesKey.createTextAttributesKey("JS.OPERATION_SIGN", DefaultLanguageHighlighterColors.OPERATION_SIGN);

	TextAttributesKey JS_PARENTHS = TextAttributesKey.createTextAttributesKey("JS.PARENTHS", DefaultLanguageHighlighterColors.PARENTHESES);

	TextAttributesKey JS_BRACKETS = TextAttributesKey.createTextAttributesKey("JS.BRACKETS", DefaultLanguageHighlighterColors.BRACKETS);

	TextAttributesKey JS_BRACES = TextAttributesKey.createTextAttributesKey("JS.BRACES", DefaultLanguageHighlighterColors.BRACES);

	TextAttributesKey JS_COMMA = TextAttributesKey.createTextAttributesKey("JS.COMMA", DefaultLanguageHighlighterColors.COMMA);

	TextAttributesKey JS_DOT = TextAttributesKey.createTextAttributesKey("JS.DOT", DefaultLanguageHighlighterColors.DOT);

	TextAttributesKey JS_SEMICOLON = TextAttributesKey.createTextAttributesKey("JS.SEMICOLON", DefaultLanguageHighlighterColors.SEMICOLON);

	TextAttributesKey JS_BAD_CHARACTER = TextAttributesKey.createTextAttributesKey("JS.BADCHARACTER", HighlighterColors.BAD_CHARACTER);
	TextAttributesKey JS_DOC_TAG = TextAttributesKey.createTextAttributesKey("JS.DOC_TAG", DefaultLanguageHighlighterColors.DOC_COMMENT_TAG);
	TextAttributesKey JS_DOC_MARKUP = TextAttributesKey.createTextAttributesKey("JS.DOC_MARKUP", DefaultLanguageHighlighterColors.DOC_COMMENT_MARKUP);
	TextAttributesKey JS_VALID_STRING_ESCAPE = TextAttributesKey.createTextAttributesKey("JS.VALID_STRING_ESCAPE",DefaultLanguageHighlighterColors.VALID_STRING_ESCAPE);
	TextAttributesKey JS_INVALID_STRING_ESCAPE = TextAttributesKey.createTextAttributesKey("JS.INVALID_STRING_ESCAPE", DefaultLanguageHighlighterColors.INVALID_STRING_ESCAPE);
	TextAttributesKey JS_LOCAL_VARIABLE = TextAttributesKey.createTextAttributesKey("JS.LOCAL_VARIABLE", DefaultLanguageHighlighterColors.LOCAL_VARIABLE);
	TextAttributesKey JS_PARAMETER = TextAttributesKey.createTextAttributesKey("JS.PARAMETER", DefaultLanguageHighlighterColors.PARAMETER);
	TextAttributesKey JS_INSTANCE_MEMBER_VARIABLE = TextAttributesKey.createTextAttributesKey("JS.INSTANCE_MEMBER_VARIABLE", DefaultLanguageHighlighterColors.INSTANCE_FIELD);
	TextAttributesKey JS_STATIC_MEMBER_VARIABLE = TextAttributesKey.createTextAttributesKey("JS.STATIC_MEMBER_VARIABLE",DefaultLanguageHighlighterColors.STATIC_FIELD);
	TextAttributesKey JS_GLOBAL_VARIABLE = TextAttributesKey.createTextAttributesKey("JS.GLOBAL_VARIABLE", DefaultLanguageHighlighterColors.STATIC_FIELD);
	TextAttributesKey JS_GLOBAL_FUNCTION = TextAttributesKey.createTextAttributesKey("JS.GLOBAL_FUNCTION", DefaultLanguageHighlighterColors.STATIC_METHOD);
	TextAttributesKey JS_STATIC_MEMBER_FUNCTION = TextAttributesKey.createTextAttributesKey("JS.STATIC_MEMBER_FUNCTION", DefaultLanguageHighlighterColors.STATIC_METHOD);
	TextAttributesKey JS_INSTANCE_MEMBER_FUNCTION = TextAttributesKey.createTextAttributesKey("JS.INSTANCE_MEMBER_FUNCTION", new TextAttributes(new RGBColor(0x7a, 0x7a, 43), StandardColors.WHITE, null, null,0));

	TextAttributesKey JS_METADATA = TextAttributesKey.createTextAttributesKey("JS.ATTRIBUTE", new TextAttributes(null, new RGBColor(0xf7, 0xe9, 0xe9), null, null, 0));
}
