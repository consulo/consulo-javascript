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

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.openapi.util.Key;
import com.intellij.psi.tree.IElementType;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 28, 2005
 * Time: 7:03:42 PM
 * To change this template use File | Settings | File Templates.
 */
public class Parsing
{
	public static final Key<JSLanguageDialect> JS_DIALECT_KEY = Key.create("JS_DIALECT");

	protected Parsing()
	{
	}

	protected static boolean checkMatches(final PsiBuilder builder, final IElementType token, final String message)
	{
		if(builder.getTokenType() == token)
		{
			builder.advanceLexer();
			return true;
		}
		else
		{
			builder.error(message);
			return false;
		}
	}

	protected static boolean isECMAL4(final PsiBuilder builder)
	{
		return builder.getUserData(JS_DIALECT_KEY) == JavaScriptSupportLoader.ECMA_SCRIPT_L4;
	}

	protected static boolean isGwt(final PsiBuilder builder)
	{
		return builder.getUserData(JS_DIALECT_KEY) == JavaScriptSupportLoader.GWT_DIALECT;
	}

	static boolean isIdentifierToken(final IElementType tokenType)
	{
		return JSTokenTypes.IDENTIFIER_TOKENS_SET.contains(tokenType);
	}
}
