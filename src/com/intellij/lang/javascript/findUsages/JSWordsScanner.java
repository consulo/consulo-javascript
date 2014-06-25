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

package com.intellij.lang.javascript.findUsages;

import com.intellij.lang.cacheBuilder.DefaultWordsScanner;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.JavaScriptLexer;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.psi.tree.TokenSet;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 31, 2005
 * Time: 9:34:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSWordsScanner extends DefaultWordsScanner
{
	public JSWordsScanner()
	{
		super(new JavaScriptLexer(JavascriptLanguage.DIALECT_OPTION_HOLDER), JSTokenTypes.IDENTIFIER_TOKENS_SET, JSTokenTypes.COMMENTS,
				TokenSet.create(JSTokenTypes.STRING_LITERAL));
		setMayHaveFileRefsInLiterals(true);
	}
}
