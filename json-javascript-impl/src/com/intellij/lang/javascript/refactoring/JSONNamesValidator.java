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

/*
 * @author max
 */
package com.intellij.lang.javascript.refactoring;

import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JSFlexAdapter;
import com.intellij.lang.javascript.JSONLexer;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.tree.IElementType;

public class JSONNamesValidator extends JSNamesValidator
{
	public JSONNamesValidator()
	{
		super(JavascriptLanguage.DIALECT_OPTION_HOLDER);
	}

	@Override
	protected Lexer createLexer(final DialectOptionHolder optionHolder)
	{
		return new JSONLexer(new JSFlexAdapter(false, optionHolder));
	}

	@Override
	public synchronized boolean isIdentifier(String name, final Project project)
	{
		if(!StringUtil.startsWithChar(name, '\'') && !StringUtil.startsWithChar(name, '\"'))
		{
			name = "\"" + name;
		}

		if(!StringUtil.endsWithChar(name, '"') && !StringUtil.endsWithChar(name, '\"'))
		{
			name += "\"";
		}

		myLexer.start(name, 0, name.length(), 0);
		IElementType type = myLexer.getTokenType();

		return myLexer.getTokenEnd() == name.length() && (type == JSTokenTypes.STRING_LITERAL || type == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL);
	}

}