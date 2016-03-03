/*
 * Copyright 2013-2016 must-be.org
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

package org.mustbe.consulo.typescript;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.lang.javascript.JavaScriptIcons;
import com.intellij.openapi.fileTypes.LanguageFileType;

/**
 * @author VISTALL
 * @since 03.03.2016
 */
public class TypeScriptFileType extends LanguageFileType
{
	public static final TypeScriptFileType INSTANCE = new TypeScriptFileType();

	private TypeScriptFileType()
	{
		super(JavaScriptLanguage.INSTANCE);
	}

	@NotNull
	@Override
	public String getName()
	{
		return "TypeScript";
	}

	@NotNull
	@Override
	public String getDescription()
	{
		return JavaScriptBundle.message("typescript.filetype.description");
	}

	@NotNull
	@Override
	public String getDefaultExtension()
	{
		return "ts";
	}

	@Nullable
	@Override
	public Icon getIcon()
	{
		return JavaScriptIcons.TypeScript;
	}
}
