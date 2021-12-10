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

package consulo.actionscript;

import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.vfs.VirtualFile;
import consulo.actionscript.lang.ActionScriptLanguageVersion;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.javascript.lang.JavaScriptFileTypeWithVersion;
import consulo.javascript.lang.JavaScriptLanguage;
import consulo.lang.LanguageVersion;
import consulo.localize.LocalizeValue;
import consulo.ui.image.Image;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 02.12.13.
 */
public class ActionScriptFileType extends LanguageFileType implements JavaScriptFileTypeWithVersion
{
	public static final ActionScriptFileType INSTANCE = new ActionScriptFileType();

	private ActionScriptFileType()
	{
		super(JavaScriptLanguage.INSTANCE);
	}

	@Nonnull
	@Override
	public String getId()
	{
		return "ACTIONSCRIPT";
	}

	@Nonnull
	@Override
	public LocalizeValue getDescription()
	{
		return LocalizeValue.localizeTODO("ActionScript files");
	}

	@Nonnull
	@Override
	public String getDefaultExtension()
	{
		return "as";
	}

	@Nullable
	@Override
	public Image getIcon()
	{
		return JavaScriptIconGroup.as();
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public LanguageVersion getLanguageVersion(@Nullable Module module, @Nullable VirtualFile virtualFile)
	{
		return ActionScriptLanguageVersion.getInstance();
	}
}
