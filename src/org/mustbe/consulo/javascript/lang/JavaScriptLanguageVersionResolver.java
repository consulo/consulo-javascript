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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.Language;
import com.intellij.lang.LanguageVersion;
import com.intellij.lang.LanguageVersionResolver;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;

/**
 * @author VISTALL
 * @since 11.12.2015
 */
public class JavaScriptLanguageVersionResolver implements LanguageVersionResolver<JavaScriptLanguage>
{
	@NotNull
	@Override
	public LanguageVersion<JavaScriptLanguage> getLanguageVersion(@NotNull Language language, @Nullable PsiElement element)
	{
		PsiFile containingFile = element == null ? null : element.getContainingFile();
		if(containingFile == null)
		{
			return JavaScript15LanguageVersion.getInstance();
		}
		return getLanguageVersion(language, element.getProject(), containingFile.getVirtualFile());
	}

	@Override
	public LanguageVersion<JavaScriptLanguage> getLanguageVersion(@NotNull Language language, @Nullable Project project, @Nullable VirtualFile virtualFile)
	{
		if(project == null || virtualFile == null)
		{
			return JavaScript15LanguageVersion.getInstance();
		}

		FileType fileType = virtualFile.getFileType();
		if(fileType instanceof JavaScriptFileTypeWithVersion)
		{
			return ((JavaScriptFileTypeWithVersion) fileType).getLanguageVersion(project, virtualFile);
		}

		return JavaScript15LanguageVersion.getInstance();
	}
}
