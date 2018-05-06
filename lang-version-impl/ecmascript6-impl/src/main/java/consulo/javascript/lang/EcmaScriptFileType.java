/*
 * Copyright 2013-2014 must-be.org
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

package consulo.javascript.lang;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import com.intellij.lang.javascript.JavaScriptIcons;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import consulo.lang.LanguageVersion;
import consulo.ui.image.Image;

/**
 * @author VISTALL
 * @since 23.12.13.
 */
public class EcmaScriptFileType extends LanguageFileType implements JavaScriptFileTypeWithVersion
{
	public static final EcmaScriptFileType INSTANCE = new EcmaScriptFileType();

	private EcmaScriptFileType()
	{
		super(JavaScriptLanguage.INSTANCE);
	}

	@Nonnull
	@Override
	public String getName()
	{
		return "ECMASCRIPT";
	}

	@Nonnull
	@Override
	public String getDescription()
	{
		return "ECMAScript files";
	}

	@Nonnull
	@Override
	public String getDefaultExtension()
	{
		return "es";
	}

	@Nullable
	@Override
	public Image getIcon()
	{
		return JavaScriptIcons.EcmaScript;
	}

	@Nonnull
	@Override
	public LanguageVersion getLanguageVersion(@Nullable Project project, @Nullable VirtualFile virtualFile)
	{
		return EcmaScript6JavaScriptVersion.getInstance();
	}
}
