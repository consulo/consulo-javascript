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
import gnu.trove.THashSet;
import gnu.trove.TIntObjectHashMap;
import gnu.trove.TObjectIntHashMap;

import java.lang.ref.SoftReference;
import java.util.Arrays;
import java.util.Collection;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.JavaScriptFileType;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.FlexModuleExtension;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.application.ApplicationManager;
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
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.MultiplePsiFilesPerDocumentFileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.xml.XmlFile;
import com.intellij.util.ArrayUtil;
import com.intellij.util.containers.HashSet;

/**
 * @by maxim, yole
 */
public final class JavaScriptIndex implements ProjectComponent
{
	private Project myProject;
	private THashMap<String, JSIndexEntry> myJavaScriptFiles = new THashMap<String, JSIndexEntry>();

	private final JSPackage myRootPackage = new JSPackage();
	private static Key<JSIndexEntry> ourEntryKey = Key.create("js.indexentry");

	private TObjectIntHashMap<String> myNames2Index = new TObjectIntHashMap<String>(50);
	private TIntObjectHashMap<String> myIndex2Names = new TIntObjectHashMap<String>(50);
	private THashSet<JSIndexEntry> myFilesToUpdate = new THashSet<JSIndexEntry>(50);

	static final Logger LOG = Logger.getInstance("#com.intellij.lang.javascript.index.JavaScriptIndex");

	@NonNls
	static final String DHTML_XML_FILE_NAME = "DHTML.xml";
	public static final String ECMASCRIPT_JS2 = "ECMAScript.js2";
	@NonNls
	static final String PREDEFINES_PREFIX = "predefines.";
	static final
	@NonNls
	String PREDEFINED_SCRIPTS_FILE_EXTENSION = ".js";
	public static final Key<String> READONLY_JS_FILE_KEY = Key.create("ReadOnly.JavaScript.File");
	public static final Key<String> PREDEFINED_JS_FILE_KEY = Key.create("Predefined.JavaScript.File");

	private static final MyEntryProcessor<Set<String>, Object> myCollectingClassNamesProcessor = new MyEntryProcessor<Set<String>, Object>()
	{
		@Override
		public void process(final JSIndexEntry entry, final Set<String> classNames, final Object o1)
		{
			entry.fillClassNames(classNames);
		}
	};
	private static final MyEntryProcessor<Set<NavigationItem>, String> myFindClassByNameProcessor = new MyEntryProcessor<Set<NavigationItem>, String>()
	{
		@Override
		public void process(final JSIndexEntry entry, final Set<NavigationItem> classes, final String name)
		{
			entry.fillClassByName(name, classes);
		}
	};

	private static final MyEntryProcessor<Set<String>, Object> myCollectSymbolNamesProcessor = new MyEntryProcessor<Set<String>, Object>()
	{
		@Override
		public void process(final JSIndexEntry entry, final Set<String> symbolNames, final Object o1)
		{
			entry.fillSymbolNames(symbolNames);
		}
	};
	private static final MyEntryProcessor<Set<NavigationItem>, String> myFindSymbolByNameProcessor = new MyEntryProcessor<Set<NavigationItem>, String>()
	{
		@Override
		public void process(final JSIndexEntry entry, final Set<NavigationItem> navigationItems, final String s)
		{
			entry.fillSymbolsByName(s, navigationItems);
		}
	};

	private final static MyEntryProcessor<Set<NavigationItem>, String> myFindFileByNameProcessor = new MyEntryProcessor<Set<NavigationItem>, String>()
	{
		@Override
		public void process(final JSIndexEntry entry, final Set<NavigationItem> navigationItems, final String s)
		{
			if(s.equals(entry.getFile().getName()))
			{
				navigationItems.add(entry.getFile());
			}
		}
	};

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
		myJavaScriptFiles.clear();
		JSTypeEvaluateManager.getInstance(myProject).clear();
		BrowserSupportManager.getInstance(myProject).clear();

		myRootPackage.clear();
		myFilesToUpdate.clear();

		synchronized(cachesLock)
		{
			myIndex2Names.clear();
			myNames2Index.clear();
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

				for(JSIndexEntry entry : myJavaScriptFiles.values())
				{
					final VirtualFile file = entry.getVirtualFile();
					final Module moduleForEntryFile = fileIndex.getModuleForFile(file);

					if(moduleForEntryFile == null)
					{
						if(flexPath != null && VfsUtil.isAncestor(flexPath, file, true))
						{
							entry.processSymbolsNoLock(processor);
						}
					}
					else if(modules.contains(moduleForEntryFile))
					{
						entry.processSymbolsNoLock(processor);
					}
				}
			}
			else
			{
				final GlobalSearchScope scope = JSResolveUtil.getSearchScope(moduleForFile, myProject);

				for(JSIndexEntry entry : myJavaScriptFiles.values())
				{
					VirtualFile file = entry.getVirtualFile();
					if(scope.contains(file))
					{
						entry.processSymbolsNoLock(processor);
					}
				}
			}
		}
		else
		{
			for(JSIndexEntry entry : myJavaScriptFiles.values())
			{
				final VirtualFile file = entry.getVirtualFile();
				final Module moduleForEntryFile = fileIndex.getModuleForFile(file);

				if(moduleForEntryFile == null)
				{ // TODO: this is not correct when more than one SDK defined
					entry.processSymbolsNoLock(processor);
					if(file == virtualFile)
					{
						seenEntryForFile = true;
					}
				}
			}
		}

		boolean contextFileForStdJs = false;

		// JS files out of module contents (except sdks) + files that have js embedded / injected (htl / jsp)
		if(((!(psiFile instanceof JSFile) ||
				psiFile.getViewProvider() instanceof MultiplePsiFilesPerDocumentFileViewProvider ||
				(contextFileForStdJs = (psiFile.getContext() != null && !ecmaL4))) && !isAcceptableFile(psiFile.getViewProvider().getVirtualFile())) ||
				(moduleForFile == null && !seenEntryForFile))
		{
			getEntryForNonJavaScriptFile(psiFile).processSymbolsNoLock(processor);
			if(contextFileForStdJs)
			{
				final PsiFile contextFile = psiFile.getContext().getContainingFile();

				if(contextFile instanceof XmlFile && XmlBackedJSClassImpl.doProcessAllTags((XmlFile) contextFile))
				{
					getEntryForNonJavaScriptFile(contextFile).processSymbolsNoLock(processor);
				}
			}
		}
	}

	private static JSIndexEntry getEntryForNonJavaScriptFile(PsiFile psiFile)
	{
		JSIndexEntry ourEntry = psiFile.getUserData(ourEntryKey);

		if(ourEntry == null)
		{
			ourEntry = new JSIndexEntry(psiFile.getViewProvider().getVirtualFile(), psiFile.getProject(), false, psiFile.getLanguage());
			psiFile.putUserData(ourEntryKey, ourEntry);
			ourEntry.setContentBelongsOnlyToMyFile();
		}
		return ourEntry;
	}

	public static boolean isFromPredefinedFile(final PsiFile containingFile)
	{
		return !containingFile.isPhysical() && containingFile.getUserData(JavaScriptIndex.PREDEFINED_JS_FILE_KEY) != null;
	}

	public JSPackage getDefaultPackage()
	{
		updateDirtyFiles();
		return myRootPackage;
	}

	private synchronized void updateDirtyFiles()
	{
		if(myFilesToUpdate != null)
		{
			THashSet<JSIndexEntry> entries = myFilesToUpdate;
			myFilesToUpdate = null;

			try
			{
				for(JSIndexEntry entry : entries)
				{
					final VirtualFile virtualfile = entry.getVirtualFile();
					if(!virtualfile.isValid())
					{
						continue; // may happen when js sources accessed from jar which was invalidated
					}
					entry.getTopLevelNsNoLock();
				}
				entries.clear();
			}
			finally
			{
				myFilesToUpdate = entries;
			}
		}
	}

	public synchronized boolean inUpdateState()
	{
		return myFilesToUpdate == null;
	}

	public synchronized PsiElement findSymbolByFileAndNameAndOffset(final String fileName, final String name, final int offset)
	{
		JSIndexEntry indexEntry = myJavaScriptFiles.get(fileName);
		if(indexEntry == null)
		{
			return null;
		}
		return findSymbolWithNameAndOffsetInEntryNoLock(getIndexOf(name), offset, indexEntry);
	}

	public synchronized PsiElement findSymbolWithNameAndOffsetInEntry(final int nameId, final int offset, final JSIndexEntry indexEntry)
	{
		return findSymbolWithNameAndOffsetInEntryNoLock(nameId, offset, indexEntry);
	}

	private PsiElement findSymbolWithNameAndOffsetInEntryNoLock(final int nameId, final int offset, final JSIndexEntry indexEntry)
	{
		if(indexEntry == null)
		{
			return null;
		}

		final PsiElement[] result = new PsiElement[1];

		indexEntry.processSymbolsNoLock(new JavaScriptSymbolProcessor.DefaultSymbolProcessor()
		{
			@Override
			public PsiFile getBaseFile()
			{
				return indexEntry.getFile();
			}

			@Override
			public int getRequiredNameId()
			{
				return nameId;
			}

			@Override
			public final boolean process(PsiElement element, JSNamespace ns)
			{
				if(element.getTextOffset() == offset)
				{
					result[0] = element;
					return false;
				}
				return true;
			}
		});
		return result[0];
	}

	public synchronized JSIndexEntry getEntryForFile(final PsiFile file)
	{
		final VirtualFile vfile = file.getViewProvider().getVirtualFile();
		if(isAcceptableFile(vfile))
		{
			final JSIndexEntry indexEntry = myJavaScriptFiles.get(vfile.getPath());
			if(indexEntry != null)
			{
				return indexEntry;
			}
		}
		return getEntryForNonJavaScriptFile(file);
	}

	public NavigationItem[] getFileByName(final String name, final boolean includeNonProjectItems)
	{
		final Set<NavigationItem> symbolNavItems = new HashSet<NavigationItem>();
		processEntries(myFindFileByNameProcessor, includeNonProjectItems, symbolNavItems, name);

		return symbolNavItems.toArray(new NavigationItem[symbolNavItems.size()]);
	}

	public final Project getProject()
	{
		return myProject;
	}

	interface MyEntryProcessor<T, T2>
	{
		void process(JSIndexEntry entry, T t, T2 t2);
	}

	private synchronized <T, T2> void processEntries(MyEntryProcessor<T, T2> processor, boolean includeNonProjectItems, T t, T2 t2)
	{
		final ProjectFileIndex fileIndex = ProjectRootManager.getInstance(myProject).getFileIndex();
		final boolean unitTestMode = ApplicationManager.getApplication().isCommandLine();
		if(unitTestMode)
		{
			includeNonProjectItems = true;
		}

		for(JSIndexEntry entry : myJavaScriptFiles.values())
		{
			final VirtualFile file = entry.getVirtualFile();
			final Module moduleForEntryFile = fileIndex.getModuleForFile(file);

			if(includeNonProjectItems || moduleForEntryFile != null)
			{
				processor.process(entry, t, t2);
			}
		}
	}

	public String[] getSymbolNames(final boolean includeNonProjectItems)
	{
		updateDirtyFiles();
		final Set<String> symbolNames = new HashSet<String>();
		processEntries(myCollectSymbolNamesProcessor, includeNonProjectItems, symbolNames, null);
		return ArrayUtil.toStringArray(symbolNames);
	}

	public NavigationItem[] getSymbolsByName(final String name, boolean includeNonProjectItems)
	{
		final Set<NavigationItem> symbolNavItems = new HashSet<NavigationItem>();
		processEntries(myFindSymbolByNameProcessor, includeNonProjectItems, symbolNavItems, name);

		return symbolNavItems.toArray(new NavigationItem[symbolNavItems.size()]);
	}

	public Collection<String> getNavigatableClassNames(final boolean includeNonProjectItems)
	{
		final Set<String> classNames = new HashSet<String>();

		processEntries(myCollectingClassNamesProcessor, includeNonProjectItems, classNames, null);

		return classNames;
	}

	public NavigationItem[] getClassByName(final String name, final boolean includeNonProjectItems)
	{
		final Set<NavigationItem> classes = new HashSet<NavigationItem>();
		processEntries(myFindClassByNameProcessor, includeNonProjectItems, classes, name);

		return classes.toArray(new NavigationItem[classes.size()]);
	}

	private static final Object cachesLock = new Object(); // guards myNames2Index, myIndexToNames, myPackageResolveResult, myToplevelResolveResult

	public int getIndexOf(@NonNls String s)
	{
		if(s == null)
		{
			return -1;
		}
		synchronized(cachesLock)
		{
			final int i = myNames2Index.get(s);
			if(i > 0)
			{
				return i;
			}
			final int value = myNames2Index.size() + 1;
			myNames2Index.put(s, value);
			myIndex2Names.put(value, s);
			return value;
		}
	}

	public String getStringByIndex(int i)
	{
		if(i == -1)
		{
			return null;
		}
		synchronized(cachesLock)
		{
			final String s = myIndex2Names.get(i);
			if(s != null)
			{
				return s;
			}
			throw new NoSuchElementException("" + i);
		}
	}

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
