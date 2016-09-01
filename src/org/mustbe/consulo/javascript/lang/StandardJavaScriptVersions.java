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

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import consulo.lang.LanguageVersion;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.ContainerUtil;

/**
 * @author VISTALL
 * @since 11.12.2015
 */
public class StandardJavaScriptVersions
{
	public static interface Marker
	{
		int getWeight();
	}

	@NotNull
	public static BaseJavaScriptLanguageVersion getDefaultVersion()
	{
		return JavaScript15LanguageVersion.getInstance();
	}

	@NotNull
	public static List<BaseJavaScriptLanguageVersion> getValidLanguageVersions()
	{
		List<BaseJavaScriptLanguageVersion> list = new ArrayList<BaseJavaScriptLanguageVersion>();
		LanguageVersion[] versions = JavaScriptLanguage.INSTANCE.getVersions();
		for(LanguageVersion version : versions)
		{
			if(version instanceof StandardJavaScriptVersions.Marker)
			{
				list.add((BaseJavaScriptLanguageVersion) version);
			}
		}

		ContainerUtil.sort(list, new Comparator<BaseJavaScriptLanguageVersion>()
		{
			@Override
			public int compare(BaseJavaScriptLanguageVersion o1, BaseJavaScriptLanguageVersion o2)
			{
				return ((StandardJavaScriptVersions.Marker) o1).getWeight() - ((StandardJavaScriptVersions.Marker) o2).getWeight();
			}
		});
		return list;
	}

	@NotNull
	public static BaseJavaScriptLanguageVersion findVersionById(@Nullable String id)
	{
		if(StringUtil.isEmpty(id))
		{
			return getDefaultVersion();
		}

		LanguageVersion[] versions = JavaScriptLanguage.INSTANCE.getVersions();
		for(LanguageVersion version : versions)
		{
			if(version instanceof StandardJavaScriptVersions.Marker && id.equals(version.getName()))
			{
				return (BaseJavaScriptLanguageVersion) version;
			}
		}
		return getDefaultVersion();
	}
}
