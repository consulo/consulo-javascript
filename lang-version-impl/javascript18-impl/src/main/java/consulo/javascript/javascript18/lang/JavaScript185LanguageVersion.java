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

package consulo.javascript.javascript18.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.ide.hightlight.JavaScriptHighlighter;
import consulo.javascript.javascript17.lang.lexer.JavaScript17Lexer;
import consulo.javascript.lang.BaseJavaScriptLanguageVersion;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.language.StandardJavaScriptVersions;
import consulo.language.editor.highlight.SyntaxHighlighter;
import consulo.language.lexer.Lexer;

import javax.annotation.Nonnull;
import java.util.function.Supplier;

/**
 * @author VISTALL
 * @since 11.12.2015
 */
@ExtensionImpl
public class JavaScript185LanguageVersion extends BaseJavaScriptLanguageVersion implements StandardJavaScriptVersions.Marker
{
	private static final Supplier<Lexer> ourLexerFactory = JavaScript17Lexer::new;

	@Nonnull
	public static JavaScript185LanguageVersion getInstance()
	{
		return JavaScriptLanguage.INSTANCE.findVersionByClass(JavaScript185LanguageVersion.class);
	}

	public JavaScript185LanguageVersion()
	{
		super("JAVASCRIPT_1_8_5");
	}

	@Nonnull
	@Override
	public String getPresentableName()
	{
		return "JavaScript 1.8.5";
	}

	@Nonnull
	@Override
	public Lexer createLexer()
	{
		return ourLexerFactory.get();
	}

	@Nonnull
	@Override
	public SyntaxHighlighter getSyntaxHighlighter()
	{
		return new JavaScriptHighlighter(ourLexerFactory);
	}

	@Override
	public int getWeight()
	{
		return 40;
	}
}