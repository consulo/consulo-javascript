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

import com.intellij.lang.javascript.psi.JSClass;
import consulo.annotation.component.ExtensionImpl;
import consulo.application.util.function.Processor;
import consulo.content.scope.SearchScope;
import consulo.ide.navigation.GotoClassOrTypeContributor;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.psi.search.FindSymbolParameters;
import consulo.language.psi.stub.IdFilter;
import consulo.language.psi.stub.StubIndex;
import consulo.navigation.NavigationItem;
import consulo.project.Project;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.Collection;

/**
 * @author VISTALL
 */
@ExtensionImpl
public class JavaScriptClassContributor implements GotoClassOrTypeContributor
{
	@Override
	public void processNames(@Nonnull Processor<String> processor, @Nonnull SearchScope searchScope, @Nullable IdFilter idFilter)
	{
		StubIndex.getInstance().processAllKeys(JavaScriptIndexKeys.CLASSES_BY_NAME, processor, (GlobalSearchScope) searchScope, idFilter);
	}

	@Override
	public void processElementsWithName(@Nonnull String name, @Nonnull Processor<NavigationItem> processor, @Nonnull FindSymbolParameters findSymbolParameters)
	{
		Project project = findSymbolParameters.getProject();
		Collection<JSClass> elements = StubIndex.getElements(JavaScriptIndexKeys.CLASSES_BY_NAME, name, project, GlobalSearchScope.allScope(project), findSymbolParameters.getIdFilter(), JSClass
				.class);

		for(JSClass element : elements)
		{
			if(!processor.process(element))
			{
				break;
			}
		}
	}
}
