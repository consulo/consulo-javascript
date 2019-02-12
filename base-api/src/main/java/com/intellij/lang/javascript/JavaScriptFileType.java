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

package com.intellij.lang.javascript;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import consulo.javascript.lang.JavaScriptFileTypeWithVersion;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.javascript.lang.StandardJavaScriptVersions;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.lang.LanguageVersion;
import consulo.ui.image.Image;

/**
 * User: max
 * Date: Jan 27, 2005
 * Time: 6:02:59 PM
 */
public class JavaScriptFileType extends LanguageFileType implements JavaScriptFileTypeWithVersion
{
	public static final JavaScriptFileType INSTANCE = new JavaScriptFileType();

	public JavaScriptFileType()
	{
		super(JavaScriptLanguage.INSTANCE);
	}

	@Override
	@Nonnull
	public String getId()
	{
		return "JavaScript";
	}

	@Override
	@Nonnull
	public String getDescription()
	{
		return JavaScriptBundle.message("javascript.filetype.description");
	}

	@Override
	@Nonnull
	public String getDefaultExtension()
	{
		return "js";
	}

	@Override
	public Image getIcon()
	{
		return JavaScriptIcons.JavaScript;
	}

	@Nonnull
	@Override
	public LanguageVersion getLanguageVersion(@Nullable Project project, @Nullable VirtualFile virtualFile)
	{
		if(virtualFile == null || project == null)
		{
			return StandardJavaScriptVersions.getInstance().getDefaultVersion();
		}
		Module moduleForFile = ModuleUtilCore.findModuleForFile(virtualFile, project);
		if(moduleForFile == null)
		{
			return StandardJavaScriptVersions.getInstance().getDefaultVersion();
		}
		JavaScriptModuleExtension<?> extension = ModuleUtilCore.getExtension(moduleForFile, JavaScriptModuleExtension.class);
		if(extension != null)
		{
			return extension.getLanguageVersion();
		}
		return StandardJavaScriptVersions.getInstance().getDefaultVersion();
	}
}
