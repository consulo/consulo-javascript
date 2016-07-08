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

import consulo.lombok.annotations.Lazy;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import org.mustbe.consulo.javascript.lang.lexer.EcmaScript6Lexer;
import org.mustbe.consulo.javascript.lang.parsing.EcmaScript6Parser;
import com.intellij.lang.PsiParser;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Factory;

/**
 * @author VISTALL
 * @since 12.12.2015
 */
public class EcmaScript6JavaScriptVersion extends BaseJavaScriptLanguageVersion implements StandardJavaScriptVersions.Marker
{
	private static final Factory<Lexer> ourLexerFactory = new Factory<Lexer>()
	{
		@Override
		public Lexer create()
		{
			return new EcmaScript6Lexer();
		}
	};

	@NotNull
	@Lazy
	public static EcmaScript6JavaScriptVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(EcmaScript6JavaScriptVersion.class);
	}

	public EcmaScript6JavaScriptVersion()
	{
		super("ECMASCRIPT_6");

		addFeature(JavaScriptFeature.CLASS);
		addFeature(JavaScriptFeature.BINARY_LITERAL);
		addFeature(JavaScriptFeature.OCTAL_LITERAL);
		addFeature(JavaScriptFeature.PARAMETER_DEFAULT_VALUE);
		addFeature(JavaScriptFeature.REST_PARAMETER);
		addFeature(JavaScriptFeature.SPREAD_OPERATOR);
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
		return ourLexerFactory.create();
	}

	@NotNull
	@Override
	public SyntaxHighlighter getSyntaxHighlighter()
	{
		return new JavaScriptHighlighter(ourLexerFactory);
	}

	@NotNull
	@Override
	public PsiParser createParser(@Nullable Project project)
	{
		return new EcmaScript6Parser();
	}

	@Override
	public int getWeight()
	{
		return 50;
	}
}
