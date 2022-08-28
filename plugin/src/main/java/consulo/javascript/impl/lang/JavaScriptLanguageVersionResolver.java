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

package consulo.javascript.impl.lang;

import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.language.JavaScriptFileTypeWithVersion;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.javascript.language.StandardJavaScriptVersions;
import consulo.language.psi.PsiFile;
import consulo.language.util.ModuleUtilCore;
import consulo.module.Module;
import consulo.project.Project;
import consulo.virtualFileSystem.VirtualFile;
import consulo.virtualFileSystem.fileType.FileType;
import consulo.language.psi.PsiElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.version.LanguageVersion;
import consulo.language.version.LanguageVersionResolver;
import consulo.language.Language;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 11.12.2015
 */
@ExtensionImpl
public class JavaScriptLanguageVersionResolver implements LanguageVersionResolver
{
	@RequiredReadAction
	@Nonnull
	@Override
	public LanguageVersion getLanguageVersion(@Nonnull Language language, @Nullable PsiElement element)
	{
		PsiFile containingFile = element == null ? null : element.getContainingFile();
		if(containingFile == null)
		{
			return StandardJavaScriptVersions.getInstance().getDefaultVersion();
		}
		FileType fileType = containingFile.getFileType();
		if(fileType instanceof JavaScriptFileTypeWithVersion)
		{
			Module module = ModuleUtilCore.findModuleForPsiElement(element);
			return ((JavaScriptFileTypeWithVersion) fileType).getLanguageVersion(module, element.getContainingFile().getVirtualFile());
		}

		return StandardJavaScriptVersions.getInstance().getDefaultVersion();
	}

	@Nonnull
	@RequiredReadAction
	@Override
	public LanguageVersion getLanguageVersion(@Nonnull Language language, @Nullable Project project, @Nullable VirtualFile virtualFile)
	{
		if(project == null || virtualFile == null)
		{
			return StandardJavaScriptVersions.getInstance().getDefaultVersion();
		}

		FileType fileType = virtualFile.getFileType();
		if(fileType instanceof JavaScriptFileTypeWithVersion)
		{
			Module module = ModuleUtilCore.findModuleForFile(virtualFile, project);
			return ((JavaScriptFileTypeWithVersion) fileType).getLanguageVersion(module, virtualFile);
		}

		return StandardJavaScriptVersions.getInstance().getDefaultVersion();
	}

	@Nonnull
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}
}
