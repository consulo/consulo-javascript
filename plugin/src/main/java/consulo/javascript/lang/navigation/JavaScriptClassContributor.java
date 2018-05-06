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

package consulo.javascript.lang.navigation;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import javax.annotation.Nonnull;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.navigation.ChooseByNameContributor;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.project.Project;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.stubs.StubIndex;
import com.intellij.util.ArrayUtil;
import com.intellij.util.indexing.IdFilter;
import consulo.javascript.lang.psi.stubs.JavaScriptIndexKeys;

/**
 * @author VISTALL
 */
public class JavaScriptClassContributor implements ChooseByNameContributor
{
	@Nonnull
	@Override
	public String[] getNames(Project project, boolean includeNonProjectItems)
	{
		final Set<String> result = new HashSet<String>();

		result.addAll(StubIndex.getInstance().getAllKeys(JavaScriptIndexKeys.CLASSES_BY_NAME, project));

		return ArrayUtil.toStringArray(result);
	}

	@Nonnull
	@Override
	public NavigationItem[] getItemsByName(String name, final String pattern, Project project, boolean includeNonProjectItems)
	{
		Collection<JSClass> elements = StubIndex.getElements(JavaScriptIndexKeys.CLASSES_BY_NAME, name, project, GlobalSearchScope.allScope(project), IdFilter.getProjectIdFilter(project,
				includeNonProjectItems), JSClass.class);
		return elements.toArray(new NavigationItem[elements.size()]);
	}
}
