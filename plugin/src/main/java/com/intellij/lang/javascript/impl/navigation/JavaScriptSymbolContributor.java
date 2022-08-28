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

package com.intellij.lang.javascript.impl.navigation;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.application.util.function.Processor;
import consulo.ide.navigation.ChooseByNameContributor;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.psi.search.FilenameIndex;
import consulo.language.psi.stub.FileBasedIndex;
import consulo.language.psi.stub.StubIndex;
import consulo.navigation.NavigationItem;
import consulo.project.Project;
import consulo.util.io.FileUtil;

import javax.annotation.Nonnull;
import java.util.Collection;
import java.util.HashSet;
import java.util.Set;

/**
 * @author maxim
 */
public class JavaScriptSymbolContributor implements ChooseByNameContributor
{
	@Nonnull
	@Override
	public String[] getNames(Project project, boolean includeNonProjectItems)
	{
		final Set<String> result = new HashSet<String>();

		result.addAll(StubIndex.getInstance().getAllKeys(JavaScriptIndexKeys.ELEMENTS_BY_NAME, project));

		FileBasedIndex.getInstance().processAllKeys(FilenameIndex.NAME, new Processor<String>()
		{
			@Override
			public boolean process(String s)
			{
				if(JavaScriptSupportLoader.isFlexMxmFile(s))
				{
					result.add(FileUtil.getNameWithoutExtension(s));
				}
				return true;
			}
		}, project);
		return result.toArray(new String[result.size()]);
	}

	@Nonnull
	@Override
	public NavigationItem[] getItemsByName(String name, final String pattern, Project project, boolean includeNonProjectItems)
	{
		GlobalSearchScope scope = includeNonProjectItems ? GlobalSearchScope.allScope(project) : GlobalSearchScope.projectScope(project);
		Collection<JSQualifiedNamedElement> result = JSResolveUtil.findElementsByName(name, project, scope);
		return result.toArray(new NavigationItem[result.size()]);
	}
}
