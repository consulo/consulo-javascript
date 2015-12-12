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
import org.mustbe.consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import org.mustbe.consulo.javascript.lang.lexer.JavaScript15Lexer;
import com.intellij.lang.javascript.JavaScriptParsingLexer;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Factory;

/**
 * @author VISTALL
 * @since 12.12.2015
 *
 * TODO [VISTALL] it used JavaScript 1.5 parsing & lexer. Need change IT!
 */
public class EcmaScript6JavaScriptVersion extends BaseJavaScriptLanguageVersion implements StandardJavaScriptVersions.Marker
{
	private static final Factory<Lexer> ourLexerFactory = new Factory<Lexer>()
	{
		@Override
		public Lexer create()
		{
			return new JavaScript15Lexer();
		}
	};

	@NotNull
	@LazyInstance
	public static EcmaScript6JavaScriptVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(EcmaScript6JavaScriptVersion.class);
	}

	public EcmaScript6JavaScriptVersion()
	{
		super("ECMASCRIPT_6");
	}

	@NotNull
	@Override
	public String getPresentableName()
	{
		return "ECMAScript 6";
	}
	@NotNull
	@Override
	public Lexer createLexer(@Nullable Project project)
	{
		return new JavaScriptParsingLexer(ourLexerFactory.create(), JavaScript15Lexer.LAST_STATE);
	}

	@NotNull
	@Override
	public SyntaxHighlighter getSyntaxHighlighter()
	{
		return new JavaScriptHighlighter(ourLexerFactory);
	}

	@Override
	public int getWeight()
	{
		return 50;
	}
}
