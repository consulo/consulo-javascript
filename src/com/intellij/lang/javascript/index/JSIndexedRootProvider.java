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

import java.util.Collections;
import java.util.Set;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.util.indexing.IndexableSetContributor;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: 24.03.2009
 * Time: 19:39:39
 * To change this template use File | Settings | File Templates.
 */
public class JSIndexedRootProvider extends IndexableSetContributor
{
	@NotNull
	@Override
	public Set<VirtualFile> getAdditionalProjectRootsToIndex(@Nullable Project project)
	{
		Set<VirtualFile> files = JavaScriptIndex.getInstance(project).getECMAScriptFilesSetFromEntries();
		for(VirtualFile file : files)
		{
			file.putUserData(JSResolveUtil.IMPLICIT_JS_FILES_KEY, Boolean.TRUE);
		}
		return files;
	}

	@Override
	public Set<VirtualFile> getAdditionalRootsToIndex()
	{
		return Collections.emptySet();
	}
}
