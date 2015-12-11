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

import java.util.Arrays;
import java.util.Set;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.FlexModuleExtension;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.search.GlobalSearchScope;

/**
 * @by maxim, yole
 */
public final class JavaScriptIndex implements ProjectComponent
{
	private Project myProject;

	public static final String ECMASCRIPT_JS2 = "ECMAScript.js2";

	public JavaScriptIndex(final Project project, final FileTypeManager fileTypeManager)
	{
		myProject = project;
	}

	@Override
	public void projectOpened()
	{
	}

	@Nullable
	public static VirtualFile getFlexSdkLocation(final Module module)
	{
		FlexModuleExtension extension = ModuleUtilCore.getExtension(module, FlexModuleExtension.class);
		if(extension != null)
		{
			Sdk sdk = extension.getSdk();
			if(sdk != null)
			{
				return sdk.getHomeDirectory();
			}
		}
		return null;
	}

	@Override
	public void projectClosed()
	{
	}

	@NotNull
	@Override
	@NonNls
	public String getComponentName()
	{
		return "JavaScriptIndex";
	}

	@Override
	public void initComponent()
	{
	}

	@Override
	public void disposeComponent()
	{
	}

	public static JavaScriptIndex getInstance(final Project project)
	{
		return project.getComponent(JavaScriptIndex.class);
	}

	public synchronized void processAllSymbols(JavaScriptSymbolProcessor processor)
	{
		assert processor.getBaseFile() != null;
		boolean ecmaL4 = processor.getBaseFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;

		final PsiFile psiFile = processor.getBaseFile();
		VirtualFile virtualFile = psiFile.getVirtualFile();
		if(virtualFile == null && psiFile.getOriginalFile() != null)
		{
			virtualFile = psiFile.getOriginalFile().getVirtualFile();
		}

		if(virtualFile == null)
		{
			return;
		}
		final ProjectFileIndex fileIndex = ProjectRootManager.getInstance(myProject).getFileIndex();

		final Module moduleForFile = fileIndex.getModuleForFile(virtualFile);
		boolean seenEntryForFile = moduleForFile != null;

		if(moduleForFile != null)
		{
			boolean facetBased = false;

			if(facetBased)
			{
				final Module[] dependencies = ModuleRootManager.getInstance(moduleForFile).getDependencies();
				final Set<Module> modules = new java.util.HashSet<Module>(dependencies.length + 1);
				modules.addAll(Arrays.asList(dependencies));
				modules.add(moduleForFile);

				VirtualFile flexPath = getFlexSdkLocation(moduleForFile);
			}
			else
			{
				final GlobalSearchScope scope = JSResolveUtil.getSearchScope(moduleForFile, myProject);

			}
		}
		else
		{

		}
	}

	public static boolean isFromPredefinedFile(final PsiFile containingFile)
	{
		return false;
	}

	public static PsiElement findSymbolByFileAndNameAndOffset(final String fileName, final String name, final int offset)
	{
		return null;
	}

	public static PsiElement findSymbolWithNameAndOffsetInEntry(final String nameId, final int offset)
	{
		return null;
	}
}
