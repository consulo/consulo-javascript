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

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.JavaScript15LanguageVersion;
import org.mustbe.consulo.javascript.lang.JavaScriptFileTypeWithVersion;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import com.intellij.lang.LanguageVersion;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;

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
	@NotNull
	public String getName()
	{
		return "JavaScript";
	}

	@Override
	@NotNull
	public String getDescription()
	{
		return JavaScriptBundle.message("javascript.filetype.description");
	}

	@Override
	@NotNull
	public String getDefaultExtension()
	{
		return "js";
	}

	@Override
	public Icon getIcon()
	{
		return JavaScriptIcons.JavaScript;
	}

	@NotNull
	@Override
	public LanguageVersion<JavaScriptLanguage> getLanguageVersion(@Nullable Project project, @Nullable VirtualFile virtualFile)
	{
		return JavaScript15LanguageVersion.getInstance();
	}
}
