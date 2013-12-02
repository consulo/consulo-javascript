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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.ASTNode;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.psi.tree.IElementType;

/**
 * @by max
 */
public class JSParser implements PsiParser
{
	private final JSLanguageDialect myDialect;

	public JSParser(@Nullable final JSLanguageDialect dialect)
	{
		myDialect = dialect;
	}

	@NotNull
	public ASTNode parse(IElementType root, PsiBuilder builder, LanguageVersion languageVersion)
	{
		final PsiBuilder.Marker rootMarker = builder.mark();
		if(myDialect == JavaScriptSupportLoader.JSON)
		{
			ExpressionParsing.parseJSON(builder);
		}
		else
		{
			builder.putUserData(Parsing.JS_DIALECT_KEY, myDialect);
			while(!builder.eof())
			{
				StatementParsing.parseSourceElement(builder);
			}
		}
		rootMarker.done(root);
		return builder.getTreeBuilt();
	}
}
