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

package com.intellij.lang.javascript.highlighting;

import org.jetbrains.annotations.Nullable;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.JSLanguageDialect;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.fileTypes.SyntaxHighlighter;
import com.intellij.openapi.fileTypes.SyntaxHighlighterFactory;
import com.intellij.openapi.fileTypes.SyntaxHighlighterProvider;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Computable;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;

public class JSSyntaxHighlighterProvider implements SyntaxHighlighterProvider
{
	@Override
	public SyntaxHighlighter create(FileType fileType, @Nullable final Project project, @Nullable final VirtualFile virtualFile)
	{
		Language lang = null;

		if(fileType != JavaScriptSupportLoader.JAVASCRIPT)
		{
			lang = JavaScriptSupportLoader.JS_IN_HTML_DIALECT;
		}

		if(virtualFile != null)
		{
			lang = JavaScriptSupportLoader.getLanguageDialect(virtualFile);
		}
		if(lang == null && project != null && virtualFile != null)
		{
			PsiFile psiFile = ApplicationManager.getApplication().runReadAction(new Computable<PsiFile>()
			{
				@Override
				public PsiFile compute()
				{
					return PsiManager.getInstance(project).findFile(virtualFile);
				}
			});

			if(psiFile != null)
			{
				lang = psiFile.getLanguage();
			}
			if(!(lang instanceof JSLanguageDialect) && !(lang instanceof JavascriptLanguage))
			{
				lang = null;
			}
		}
		if(lang == null)
		{
			lang = ((LanguageFileType) fileType).getLanguage();
		}
		return SyntaxHighlighterFactory.getSyntaxHighlighter(lang, project, virtualFile);
	}
}