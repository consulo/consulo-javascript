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

import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.icon.JavaScriptIconGroup;
import consulo.javascript.module.extension.JavaScriptModuleExtension;
import consulo.lang.LanguageVersion;
import consulo.ui.image.Image;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

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
	public String getId()
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
		return JavaScriptIconGroup.ecmaScript();
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public LanguageVersion getLanguageVersion(@Nullable Module module, @Nullable VirtualFile virtualFile)
	{
		if(module == null)
		{
			return EcmaScript12JavaScriptVersion.getInstance();
		}

		JavaScriptModuleExtension<?> extension = ModuleUtilCore.getExtension(module, JavaScriptModuleExtension.class);
		if(extension != null)
		{
			return extension.getLanguageVersion();
		}
		return EcmaScript12JavaScriptVersion.getInstance();
	}
}
