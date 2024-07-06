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

package com.intellij.lang.javascript.parsing;

import com.intellij.lang.javascript.JSDocTokenTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.ast.IElementType;
import consulo.language.parser.PsiBuilder;
import org.jetbrains.annotations.NonNls;

/**
 * @author Maxim.Mossienko
 *         Date: Aug 5, 2008
 *         Time: 3:55:54 PM
 */
public class JSDocParsing
{
	public static void parseJSDoc(final PsiBuilder builder)
	{
		final PsiBuilder.Marker root = builder.mark();

		while(!builder.eof())
		{
			final IElementType tokenType = builder.getTokenType();

			if(tokenType == JSDocTokenTypes.DOC_TAG_NAME)
			{
				if(parseDocTag(builder))
				{
					continue;
				}
			}

			builder.advanceLexer();
		}

		root.done(JSTokenTypes.DOC_COMMENT);
	}

	private static boolean parseDocTag(final PsiBuilder builder)
	{
		assert builder.getTokenType() == JSDocTokenTypes.DOC_TAG_NAME;
		final PsiBuilder.Marker docTagMarker = builder.mark();

		try
		{
			final @NonNls String tagName = builder.getTokenText();

			builder.advanceLexer();
			if("@param".equals(tagName))
			{

				if(isInvalidTokenType(builder))
				{
					builder.error(JavaScriptLocalize.javascriptParserMessageExpectedDocTagName());
					return false;
				}

				String currentText = builder.getTokenText();
				if(currentText != null && currentText.startsWith("{"))
				{
					createDocTagValue(builder);
					builder.getTokenType();
				}
				builder.advanceLexer();
				currentText = builder.getTokenText();
				if(currentText != null)
				{
					if(currentText.equals(":"))
					{
						builder.advanceLexer();
					}
					else if(!currentText.startsWith("{"))
					{
						return true;
					}
				}

				if(isInvalidTokenType(builder))
				{
					return true;
				}
			}
			else
			{
				final boolean hasDocTagValue = isToCreateDocTagValue(tagName);
				if(!hasDocTagValue)
				{
					return true;
				}

				if(isInvalidTokenType(builder))
				{
					builder.error(JavaScriptLocalize.javascriptParserMessageExpectedDocTagValue());
					return false;
				}
			}

			createDocTagValue(builder);
		}
		finally
		{
			docTagMarker.done(JSDocTokenTypes.DOC_TAG);
		}

		return true;
	}

	private static void createDocTagValue(final PsiBuilder builder)
	{
		PsiBuilder.Marker marker = builder.mark();
		builder.advanceLexer();
		marker.done(JSDocTokenTypes.DOC_TAG_VALUE);
	}

	private static boolean isInvalidTokenType(final PsiBuilder builder)
	{
		final IElementType tokenType = builder.getTokenType();
		return builder.eof() || tokenType == JSDocTokenTypes.DOC_COMMENT_LEADING_ASTERISK || tokenType == JSDocTokenTypes.DOC_COMMENT_END;
	}

	private static boolean isToCreateDocTagValue(final @NonNls String tokenText)
	{
		return tokenText.equals("@see") ||
				tokenText.equals("@class") ||
				tokenText.equals("@member") ||
				tokenText.equals("@requires") ||
				tokenText.equals("@type") ||
				tokenText.equals("@copy") ||
				tokenText.equals("@extends") ||
				tokenText.equals("@base") ||
				tokenText.equals("@base") ||
				tokenText.equals("@returns");
	}
}
