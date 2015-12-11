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

import gnu.trove.THashMap;

import java.lang.ref.SoftReference;
import java.util.Arrays;
import java.util.Map;
import java.util.Set;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.FlexModuleExtension;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;

/**
 * @by maxim, yole
 */
public final class JavaScriptIndex implements ProjectComponent
{
	private Project myProject;
	static final Logger LOG = Logger.getInstance("#com.intellij.lang.javascript.index.JavaScriptIndex");

	@NonNls
	static final String DHTML_XML_FILE_NAME = "DHTML.xml";
	public static final String ECMASCRIPT_JS2 = "ECMAScript.js2";

	@Deprecated
	public static final Key<String> PREDEFINED_JS_FILE_KEY = Key.create("Predefined.JavaScript.File");


	private final GlobalSearchScope myScopeForPackages;

	public JavaScriptIndex(final Project project, final FileTypeManager fileTypeManager)
	{
		myProject = project;
		myScopeForPackages = GlobalSearchScope.allScope(project);
	}

	@Override
	public void projectOpened()
	{
	}

	private boolean isAcceptableFile(final VirtualFile fileOrDir)
	{
		return fileOrDir.getFileType() == JavaScriptFileType.INSTANCE;
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
		clear();
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

	public synchronized void clear()
	{
		synchronized(cachesLock)
		{
			myPackageResolveResult.clear();
			myTopLevelResolveResult.clear();
		}
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
		return !containingFile.isPhysical() && containingFile.getUserData(JavaScriptIndex.PREDEFINED_JS_FILE_KEY) != null;
	}

	public boolean inUpdateState()
	{
		return true;
	}

	public synchronized PsiElement findSymbolByFileAndNameAndOffset(final String fileName, final String name, final int offset)
	{
		return null;
	}

	public synchronized PsiElement findSymbolWithNameAndOffsetInEntry(final String nameId, final int offset)
	{
		return null;
	}

	public final Project getProject()
	{
		return myProject;
	}

	private static final Object cachesLock = new Object(); // guards myNames2Index, myIndexToNames, myPackageResolveResult, myToplevelResolveResult


	private final Map<GlobalSearchScope, Map<String, PsiElement>> myPackageResolveResult = new THashMap<GlobalSearchScope, Map<String, PsiElement>>();
	private final Map<GlobalSearchScope, Map<String, SoftReference<PsiElement>>> myTopLevelResolveResult = new THashMap<GlobalSearchScope, Map<String,
			SoftReference<PsiElement>>>();
	private long modificationStamp;

	public void rememberPackageElement(final String link, final @NotNull PsiElement psiElement)
	{
		synchronized(cachesLock)
		{
			Map<String, PsiElement> map = myPackageResolveResult.get(myScopeForPackages);
			if(map == null)
			{
				map = new THashMap<String, PsiElement>();
				myPackageResolveResult.put(myScopeForPackages, map);
			}
			map.put(link, psiElement);
		}
	}

	public void rememberTopLevelClassElement(final String link, GlobalSearchScope scope, final @NotNull PsiElement psiElement)
	{
		synchronized(cachesLock)
		{
			Map<String, SoftReference<PsiElement>> map = myTopLevelResolveResult.get(scope);
			if(map == null)
			{
				map = new THashMap<String, SoftReference<PsiElement>>();
				myTopLevelResolveResult.put(scope, map);
			}

			map.put(link, new SoftReference<PsiElement>(psiElement));
		}
	}

	public
	@Nullable
	PsiElement recallPackageElement(final String link)
	{
		synchronized(cachesLock)
		{
			return doRecall(myPackageResolveResult, myScopeForPackages, myProject, link);
		}
	}

	public
	@Nullable
	PsiElement recallClass(final String link, GlobalSearchScope scope)
	{
		synchronized(cachesLock)
		{
			SoftReference<PsiElement> elementSoftReference = doRecall(myTopLevelResolveResult, scope, myProject, link);
			return elementSoftReference != null ? elementSoftReference.get() : null;
		}
	}

	private
	@Nullable
	<T> T doRecall(final Map<GlobalSearchScope, Map<String, T>> packageResolveResult, final GlobalSearchScope scope, Project project, final String link)
	{
		final long count = PsiManager.getInstance(project).getModificationTracker().getModificationCount();
		if(count != modificationStamp)
		{
			modificationStamp = count;
			myPackageResolveResult.clear();
			myTopLevelResolveResult.clear();
			return null;
		}
		Map<String, T> map = packageResolveResult.get(scope);
		if(map != null)
		{
			return map.get(link);
		}
		return null;
	}
}
