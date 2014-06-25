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

package com.intellij.lang.javascript;

import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;

/**
 * @by maxim.mossienko
 */
public interface JSDocTokenTypes
{
	IElementType DOC_COMMENT_START = new JSDocElementType("DOC_COMMENT_START");
	IElementType DOC_COMMENT_END = new JSDocElementType("DOC_COMMENT_END");
	IElementType DOC_SPACE = new JSDocElementType("DOC_SPACE");
	IElementType DOC_REAL_WHITESPACE = TokenType.WHITE_SPACE;
	IElementType DOC_COMMENT_DATA = new JSDocElementType("DOC_COMMENT_DATA");
	IElementType DOC_COMMENT_LEADING_ASTERISK = new JSDocElementType("DOC_COMMENT_LEADING_ASTERISK");
	IElementType DOC_TAG_NAME = new JSDocElementType("DOC_TAG_NAME");
	IElementType DOC_BAD_CHARACTER = TokenType.BAD_CHARACTER;
	IElementType DOC_TAG_VALUE = new JSDocElementType("DOC_TAG_VALUE");

	TokenSet TOKENS_TO_MERGE = TokenSet.create(DOC_SPACE, DOC_REAL_WHITESPACE);
	IElementType DOC_TAG = new JSDocElementType("DOC_TAG");

	IElementType DOC_TAG_VALUE_LPAREN = new JSDocElementType("DOC_TAG_VALUE_LPAREN");
	IElementType DOC_TAG_VALUE_RPAREN = new JSDocElementType("DOC_TAG_VALUE_RPAREN");
	IElementType DOC_TAG_VALUE_SHARP = new JSDocElementType("DOC_TAG_VALUE_SHARP");
	IElementType DOC_TAG_VALUE_COMMA = new JSDocElementType("DOC_TAG_VALUE_COMMA");
	IElementType DOC_TAG_VALUE_LT = new JSDocElementType("DOC_TAG_VALUE_LT");
	IElementType DOC_TAG_VALUE_GT = new JSDocElementType("DOC_TAG_VALUE_GT");
	IElementType INLINE_TAG_START = new JSDocElementType("DOC_INLINE_TAG_START");
	IElementType INLINE_TAG_END = new JSDocElementType("DOC_INLINE_TAG_END");
}