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

package com.intellij.lang.javascript.index;

import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.util.ArrayUtil;

/**
 * Created by IntelliJ IDEA.
 * User: yole
 * Date: 03.10.2005
 * Time: 14:45:22
 * To change this template use File | Settings | File Templates.
 */
public class JavaScriptSymbolContributor implements ChooseByNameContributor
{
	@Override
	public String[] getNames(Project project, boolean includeNonProjectItems)
	{
		JavaScriptIndex index = JavaScriptIndex.getInstance(project);
		return index != null ? index.getSymbolNames(includeNonProjectItems) : ArrayUtil.EMPTY_STRING_ARRAY;
	}

	@Override
	public NavigationItem[] getItemsByName(String name, final String pattern, Project project, boolean includeNonProjectItems)
	{
		JavaScriptIndex index = JavaScriptIndex.getInstance(project);
		return index != null ? index.getSymbolsByName(name, includeNonProjectItems) : NavigationItem.EMPTY_NAVIGATION_ITEM_ARRAY;
	}
}
