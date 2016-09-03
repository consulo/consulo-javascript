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

package consulo.json;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.javascript.lang.JavaScriptFileTypeWithVersion;
import org.mustbe.consulo.javascript.lang.JavaScriptLanguage;
import consulo.json.lang.JsonJavaScriptVersion;
import com.intellij.lang.javascript.JavaScriptIcons;
import com.intellij.openapi.fileTypes.LanguageFileType;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;

/**
 * @author VISTALL
 * @since 02.12.13.
 */
public class JsonFileType extends LanguageFileType implements JavaScriptFileTypeWithVersion
{
	public static final JsonFileType INSTANCE = new JsonFileType();

	private JsonFileType()
	{
		super(JavaScriptLanguage.INSTANCE);
	}

	@NotNull
	@Override
	public String getName()
	{
		return "JSON";
	}

	@NotNull
	@Override
	public String getDescription()
	{
		return "Json files";
	}

	@NotNull
	@Override
	public String getDefaultExtension()
	{
		return "json";
	}

	@Nullable
	@Override
	public Icon getIcon()
	{
		return JavaScriptIcons.Json;
	}

	@NotNull
	@Override
	public JsonJavaScriptVersion getLanguageVersion(@Nullable Project project, @Nullable VirtualFile virtualFile)
	{
		return JsonJavaScriptVersion.getInstance();
	}
}
