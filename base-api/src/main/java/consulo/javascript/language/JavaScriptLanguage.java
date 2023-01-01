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

package consulo.javascript.language;

import consulo.language.Language;
import consulo.language.file.LanguageFileType;
import consulo.language.version.LanguageVersion;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.HashMap;
import java.util.Map;

/**
 * @author VISTALL
 * @since 05.12.2015
 */
public class JavaScriptLanguage extends Language
{
	public static final JavaScriptLanguage INSTANCE = new JavaScriptLanguage();

	private Map<String, LanguageVersion> myVersionsById = new HashMap<>();

	public JavaScriptLanguage()
	{
		super("JavaScript", "text/javascript", "application/javascript");
	}

	@Nullable
	public LanguageVersion getVersionById(@Nonnull String id)
	{
		Map<String, LanguageVersion> oldMap = myVersionsById;
		if(oldMap == null)
		{
			Map<String, LanguageVersion> newMap = new HashMap<>();
			for(LanguageVersion version : getVersions())
			{
				newMap.put(version.getId(), version);
			}

			myVersionsById = newMap;

			return newMap.get(id);
		}

		return oldMap.get(id);
	}

	@Nullable
	@Override
	public LanguageFileType getAssociatedFileType()
	{
		return JavaScriptFileType.INSTANCE;
	}

	@Override
	public boolean isCaseSensitive()
	{
		return true;
	}
}
