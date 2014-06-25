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
package com.intellij.lang.javascript;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.parsing.JSParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;
import com.intellij.psi.tree.IFileElementType;

public class GwtParserDefinition extends JavascriptParserDefinition
{
	@Override
	@NotNull
	public Lexer createLexer(final Project project, LanguageVersion languageVersion)
	{
		return new JavaScriptParsingLexer(GwtLanguageDialect.DIALECT_OPTION_HOLDER);
	}

	@Override
	public IFileElementType getFileNodeType()
	{
		return JSElementTypes.GWT_FILE;
	}


	@Override
	@NotNull
	public PsiParser createParser(final Project project, LanguageVersion languageVersion)
	{
		return new JSParser(JavaScriptSupportLoader.GWT_DIALECT);
	}
}