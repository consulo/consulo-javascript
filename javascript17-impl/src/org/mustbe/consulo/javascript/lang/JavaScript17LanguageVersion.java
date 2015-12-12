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

package org.mustbe.consulo.javascript.lang;

import org.consulo.lombok.annotations.LazyInstance;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.DialectOptionHolder;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lang.javascript.highlighting.JSHighlighter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.project.Project;

/**
 * @author VISTALL
 * @since 11.12.2015
 */
public class JavaScript17LanguageVersion extends BaseJavaScriptLanguageVersion implements StandardJavaScriptVersions.Marker
{
	@NotNull
	@LazyInstance
	public static JavaScript17LanguageVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(JavaScript17LanguageVersion.class);
	}

	public JavaScript17LanguageVersion()
	{
		super("JAVASCRIPT_1_7");
	}

	@NotNull
	@Override
	public String getPresentableName()
	{
		return "JavaScript 1.7";
	}

	@NotNull
	@Override
	public Lexer createLexer(@Nullable Project project)
	{
		return new JavaScriptParsingLexer(DialectOptionHolder.dummy());
	}

	@NotNull
	@Override
	public JSHighlighter getSyntaxHighlighter()
	{
		return new JSHighlighter(DialectOptionHolder.dummy());
	}

	@Override
	public int getWeight()
	{
		return 2;
	}
}