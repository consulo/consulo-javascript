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

package consulo.javascript.impl.lang.navigation;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

import javax.annotation.Nonnull;

import com.intellij.lang.javascript.psi.JSClass;
import consulo.ide.navigation.ChooseByNameContributor;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.psi.stub.IdFilter;
import consulo.language.psi.stub.StubIndex;
import consulo.util.collection.ArrayUtil;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.navigation.NavigationItem;
import consulo.project.Project;

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
