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
import consulo.annotation.component.ExtensionImpl;
import consulo.application.util.function.Processor;
import consulo.content.scope.SearchScope;
import consulo.ide.navigation.GotoSymbolContributor;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.psi.search.FilenameIndex;
import consulo.language.psi.search.FindSymbolParameters;
import consulo.language.psi.stub.FileBasedIndex;
import consulo.language.psi.stub.IdFilter;
import consulo.language.psi.stub.StubIndex;
import consulo.navigation.NavigationItem;
import consulo.project.Project;
import consulo.util.io.FileUtil;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import java.util.Collection;

/**
 * @author maxim
 */
@ExtensionImpl
public class JavaScriptSymbolContributor implements GotoSymbolContributor
{
	@Override
	public void processNames(@Nonnull Processor<String> processor, @Nonnull SearchScope searchScope, @Nullable IdFilter idFilter)
	{
		StubIndex.getInstance().processAllKeys(JavaScriptIndexKeys.ELEMENTS_BY_NAME, processor, (GlobalSearchScope) searchScope, idFilter);

		FileBasedIndex.getInstance().processAllKeys(FilenameIndex.NAME, new Processor<String>()
		{
			@Override
			public boolean process(String s)
			{
				if(JavaScriptSupportLoader.isFlexMxmFile(s))
				{
					return processor.process(FileUtil.getNameWithoutExtension(s));
				}
				return true;
			}
		}, searchScope, idFilter);
	}

	@Override
	public void processElementsWithName(@Nonnull String name, @Nonnull Processor<NavigationItem> processor, @Nonnull FindSymbolParameters findSymbolParameters)
	{
		Project project = findSymbolParameters.getProject();

		GlobalSearchScope scope = (GlobalSearchScope) findSymbolParameters.getSearchScope();

		Collection<JSQualifiedNamedElement> result = JSResolveUtil.findElementsByName(name, project, scope);

		for(JSQualifiedNamedElement element : result)
		{
			if(!processor.process(element))
			{
				break;
			}
		}
	}
}
