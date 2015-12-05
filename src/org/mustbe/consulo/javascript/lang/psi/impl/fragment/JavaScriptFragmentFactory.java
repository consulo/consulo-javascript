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

package org.mustbe.consulo.javascript.lang.psi.impl.fragment;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.javascript.lang.parsing.ExpressionParsing;
import org.mustbe.consulo.javascript.lang.parsing.JavaScriptParsingContext;
import com.intellij.lang.ASTNode;
import com.intellij.lang.Language;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiBuilderFactory;
import com.intellij.lang.PsiParser;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.SingleRootFileViewProvider;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.IFileElementType;
import com.intellij.testFramework.LightVirtualFile;

/**
 * @author VISTALL
 * @since 05.12.2015
 */
public class JavaScriptFragmentFactory
{
	private static final PsiParser ourExpressionParser = new PsiParser()
	{
		@NotNull
		@Override
		public ASTNode parse(@NotNull IElementType elementType, @NotNull PsiBuilder builder, @NotNull LanguageVersion languageVersion)
		{
			PsiBuilder.Marker mark = builder.mark();
			JavaScriptParsingContext context = new JavaScriptParsingContext();
			ExpressionParsing expressionParsing = new ExpressionParsing(context);
			expressionParsing.parseExpression(builder);
			while(!builder.eof())
			{
				builder.error("Unexpected token");
				builder.advanceLexer();
			}
			mark.done(elementType);
			return builder.getTreeBuilt();
		}
	};

	private static final IFileElementType ourExpressionFileElementType = new IFileElementType("CSHARP_EXPRESSION_FRAGMENT", JavascriptLanguage.INSTANCE)
	{
		@Override
		protected ASTNode doParseContents(@NotNull final ASTNode chameleon, @NotNull final PsiElement psi)
		{
			final Project project = psi.getProject();
			final Language languageForParser = getLanguageForParser(psi);
			final LanguageVersion tempLanguageVersion = chameleon.getUserData(LanguageVersion.KEY);
			final LanguageVersion languageVersion = tempLanguageVersion == null ? psi.getLanguageVersion() : tempLanguageVersion;
			final PsiBuilder builder = PsiBuilderFactory.getInstance().createBuilder(project, chameleon, null, languageForParser, languageVersion, chameleon.getChars());

			return ourExpressionParser.parse(this, builder, languageVersion);
		}
	};

	@NotNull
	public static PsiFile createExpressionFragment(@NotNull Project project, @NotNull CharSequence text)
	{
		LightVirtualFile virtualFile = new LightVirtualFile("dummy.cs", JavaScriptFileType.INSTANCE, text, System.currentTimeMillis());
		SingleRootFileViewProvider viewProvider = new SingleRootFileViewProvider(PsiManager.getInstance(project), virtualFile, true)
		{
			@NotNull
			@Override
			public SingleRootFileViewProvider createCopy(@NotNull final VirtualFile copy)
			{
				SingleRootFileViewProvider provider = new SingleRootFileViewProvider(getManager(), copy, false);
				JavaScriptFragmentFileImpl psiFile = new JavaScriptFragmentFileImpl(ourExpressionFileElementType, provider);
				provider.forceCachedPsi(psiFile);
				psiFile.getNode();
				return provider;
			}
		};
		JavaScriptFragmentFileImpl file = new JavaScriptFragmentFileImpl(ourExpressionFileElementType, viewProvider);
		viewProvider.forceCachedPsi(file);
		file.getNode();
		return file;
	}
}
