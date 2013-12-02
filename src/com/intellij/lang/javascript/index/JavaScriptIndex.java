package com.intellij.lang.javascript.index;

import gnu.trove.THashMap;
import gnu.trove.THashSet;
import gnu.trove.TIntObjectHashMap;
import gnu.trove.TObjectIntHashMap;
import gnu.trove.TObjectIntIterator;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.lang.ref.SoftReference;
import java.lang.reflect.Method;
import java.net.URL;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.Set;

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.ProjectTopics;
import com.intellij.javaee.UriUtil;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.flex.FlexModuleExtension;
import com.intellij.lang.javascript.flex.XmlBackedJSClassImpl;
import com.intellij.lang.javascript.index.predefined.Marker;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.navigation.NavigationItem;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.application.PathManager;
import com.intellij.openapi.components.ProjectComponent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileTypes.FileTypeManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleManager;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.progress.ProgressIndicator;
import com.intellij.openapi.progress.ProgressManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ContentIterator;
import com.intellij.openapi.roots.ModuleRootEvent;
import com.intellij.openapi.roots.ModuleRootListener;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.startup.StartupManager;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.vfs.VfsUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.VirtualFileAdapter;
import com.intellij.openapi.vfs.VirtualFileEvent;
import com.intellij.openapi.vfs.VirtualFileListener;
import com.intellij.openapi.vfs.VirtualFileManager;
import com.intellij.openapi.vfs.VirtualFileMoveEvent;
import com.intellij.openapi.vfs.VirtualFilePropertyEvent;
import com.intellij.openapi.vfs.newvfs.NewVirtualFile;
import com.intellij.psi.MultiplePsiFilesPerDocumentFileViewProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiTreeChangeAdapter;
import com.intellij.psi.PsiTreeChangeEvent;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.xml.XmlFile;
import com.intellij.util.ArrayUtil;
import com.intellij.util.Processor;
import com.intellij.util.containers.HashSet;
import com.intellij.util.messages.MessageBusConnection;

/**
 * @by maxim, yole
 */
public final class JavaScriptIndex implements ProjectComponent
{
	private Project myProject;
	private final FileTypeManager myFileTypeManager;
	private THashMap<String, JSIndexEntry> myOldJavaScriptFiles;
	private THashMap<String, JSIndexEntry> myJavaScriptFiles = new THashMap<String, JSIndexEntry>();
	private THashMap<String, JSIndexEntry> myPredefinedJavaScriptFiles = new THashMap<String, JSIndexEntry>();
	private final THashSet<VirtualFile> myPredefinedJavaScriptVirtualFiles = new THashSet<VirtualFile>();

	private final JSPackage myRootPackage = new JSPackage();
	private static Key<JSIndexEntry> ourEntryKey = Key.create("js.indexentry");

	private JSTreeChangeListener myTreeChangeListener;
	private final Set<VirtualFile> setOfExternalDefsRoots = new java.util.HashSet<VirtualFile>();
	private VirtualFileListener myFileListener;

	private Runnable myUpdateRunnable;
	private boolean myLoadingProject;
	private boolean myDoingFilesRescan;

	private TObjectIntHashMap<String> myNames2Index = new TObjectIntHashMap<String>(50);
	private TIntObjectHashMap<String> myIndex2Names = new TIntObjectHashMap<String>(50);
	private THashSet<JSIndexEntry> myFilesToUpdate = new THashSet<JSIndexEntry>(50);

	@NonNls
	private static final String JS_CACHES_DIR_NAME = "js_caches";
	private static final byte CURRENT_VERSION = 127;
	static final Logger LOG = Logger.getInstance("#com.intellij.lang.javascript.index.JavaScriptIndex");

	@NonNls
	static final String DHTML_XML_FILE_NAME = "DHTML.xml";
	public static final String ECMASCRIPT_JS2 = "ECMAScript.js2";
	private static
	@NonNls
	Set<String> ourPredefinedFileNames = new LinkedHashSet<String>(Arrays.asList(ECMASCRIPT_JS2, "DOMCore.xml", DHTML_XML_FILE_NAME, "AJAX.xml",
			"DOMEvents.xml", "DOMTraversalAndRange.xml", "DOMXPath.xml", "E4X.js2"));
	@NonNls
	static final String PREDEFINES_PREFIX = "predefines.";
	private MessageBusConnection myConnection;
	static final
	@NonNls
	String PREDEFINED_SCRIPTS_FILE_EXTENSION = ".js";
	public static final Key<String> READONLY_JS_FILE_KEY = Key.create("ReadOnly.JavaScript.File");
	public static final Key<String> PREDEFINED_JS_FILE_KEY = Key.create("Predefined.JavaScript.File");

	private static final MyEntryProcessor<Set<String>, Object> myCollectingClassNamesProcessor = new MyEntryProcessor<Set<String>, Object>()
	{
		public void process(final JSIndexEntry entry, final Set<String> classNames, final Object o1)
		{
			entry.fillClassNames(classNames);
		}
	};
	private static final MyEntryProcessor<Set<NavigationItem>, String> myFindClassByNameProcessor = new MyEntryProcessor<Set<NavigationItem>, String>()
	{
		public void process(final JSIndexEntry entry, final Set<NavigationItem> classes, final String name)
		{
			entry.fillClassByName(name, classes);
		}
	};

	private static final MyEntryProcessor<Set<String>, Object> myCollectSymbolNamesProcessor = new MyEntryProcessor<Set<String>, Object>()
	{
		public void process(final JSIndexEntry entry, final Set<String> symbolNames, final Object o1)
		{
			entry.fillSymbolNames(symbolNames);
		}
	};
	private static final MyEntryProcessor<Set<NavigationItem>, String> myFindSymbolByNameProcessor = new MyEntryProcessor<Set<NavigationItem>, String>()
	{
		public void process(final JSIndexEntry entry, final Set<NavigationItem> navigationItems, final String s)
		{
			entry.fillSymbolsByName(s, navigationItems);
		}
	};

	private final static MyEntryProcessor<Set<NavigationItem>, String> myFindFileByNameProcessor = new MyEntryProcessor<Set<NavigationItem>, String>()
	{
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
		myFileTypeManager = fileTypeManager;
		myTreeChangeListener = new JSTreeChangeListener();
		PsiManager.getInstance(myProject).addPsiTreeChangeListener(myTreeChangeListener);
		myScopeForPackages = GlobalSearchScope.allScope(project);
	}

	public void projectOpened()
	{
		myLoadingProject = true;
		myUpdateRunnable = new Runnable()
		{
			public void run()
			{
				try
				{
					final ProgressIndicator progress = ProgressManager.getInstance().getProgressIndicator();
					myDoingFilesRescan = true;

					if(progress != null)
					{
						progress.pushState();
						progress.setIndeterminate(true);
						progress.setText(JSBundle.message("building.index.message"));
					}

					processModulesThatContainExternalJSDefinitions(new Processor<VirtualFile>()
					{
						public boolean process(final VirtualFile home)
						{
							if(home.isDirectory())
							{
								setOfExternalDefsRoots.add(home);
							}
							return true;
						}
					}, myProject);

					if(myLoadingProject)
					{
						loadCaches(progress);
					}

					final ProjectFileIndex fileIndex = ProjectRootManager.getInstance(myProject).getFileIndex();
					final List<VirtualFile> filesToProcess = new ArrayList<VirtualFile>(5);

					fileIndex.iterateContent(new ContentIterator()
					{
						public boolean processFile(VirtualFile fileOrDir)
						{
							if(isAcceptableFile(fileOrDir) &&
									myJavaScriptFiles.get(fileOrDir.getPath()) == null &&
									!fileOrDir.isDirectory())
							{
								filesToProcess.add(fileOrDir);
							}
							return true;
						}
					});

					processModulesThatContainExternalJSDefinitions(new Processor<VirtualFile>()
					{
						public boolean process(final VirtualFile home)
						{
							collectFilesUnderDirectory(home, filesToProcess, !ApplicationManager.getApplication().isCommandLine());
							return true;
						}
					}, myProject);

					if(progress != null)
					{
						progress.setIndeterminate(false);
					}

					int processed = 0;

					for(VirtualFile f : filesToProcess)
					{
						fileAdded(f);

						++processed;
						if(progress != null)
						{
							progress.setFraction((double) processed / filesToProcess.size());
							ProgressManager.getInstance().checkCanceled();
						}
					}

					if(progress != null)
					{
						progress.setFraction(1.0);
						progress.setText("");
						progress.setText2("");
						progress.popState();
					}
				}
				finally
				{
					myLoadingProject = false;
					myDoingFilesRescan = false;
				}
			}
		};

		if(ApplicationManager.getApplication().isCommandLine())
		{
			if(myProject.isOpen())
			{
				StartupManager.getInstance(myProject).registerStartupActivity(myUpdateRunnable);
			}
			else
			{
				clear();
				myUpdateRunnable.run();
			}
		}
		else
		{
			StartupManager.getInstance(myProject).registerStartupActivity(myUpdateRunnable);
		}

		myFileListener = new VirtualFileAdapter()
		{
			final Method fileAddedCallback;
			final Method fileChangedCallback;
			final Method fileRemovedCallback;
			final ProjectFileIndex fileIndex = ProjectRootManager.getInstance(myProject).getFileIndex();
			final boolean unitTestMode = ApplicationManager.getApplication().isCommandLine();

			{
				try
				{
					fileAddedCallback = JavaScriptIndex.class.getDeclaredMethod("fileAdded", VirtualFile.class);
					fileAddedCallback.setAccessible(true);
					fileRemovedCallback = JavaScriptIndex.class.getDeclaredMethod("fileRemoved", VirtualFile.class);
					fileRemovedCallback.setAccessible(true);

					fileChangedCallback = JavaScriptIndex.class.getDeclaredMethod("fileChanged", VirtualFile.class);
					fileChangedCallback.setAccessible(true);
				}
				catch(Exception e)
				{
					throw new RuntimeException(e);
				}
			}

			public void fileCreated(VirtualFileEvent event)
			{
				if(this != myFileListener)
				{
					return; // disposed
				}
				propagateEvent(event.getFile(), fileAddedCallback, false);
			}

			private void propagateEvent(final VirtualFile file, Method callback, boolean propagateViaCachedChildren)
			{
				if(!file.isInLocalFileSystem() || myProject.isDisposed() || (!unitTestMode && !myProject.isInitialized()) || fileIndex.isIgnored(file))
				{
					return;
				}
				final VirtualFile contentRoot = fileIndex.getContentRootForFile(file);

				if(contentRoot == null)
				{
					if(callback == fileAddedCallback)
					{
						return;
					}

					if(!isAcceptableFile(file) && !file.isDirectory())
					{
						return;
					}
					if(!ApplicationManager.getApplication().isCommandLine())
					{
						boolean hasSdkAncestor = false;

						for(VirtualFile root : setOfExternalDefsRoots)
						{
							if(VfsUtil.isAncestor(root, file, true))
							{
								hasSdkAncestor = true;
								break;
							}
						}

						if(!hasSdkAncestor)
						{
							return;
						}
					}
				}

				if(file.isDirectory())
				{
					if(propagateViaCachedChildren && file instanceof NewVirtualFile)
					{
						for(VirtualFile child : ((NewVirtualFile) file).getCachedChildren())
						{
							propagateEvent(child, callback, propagateViaCachedChildren);
						}
					}
					else
					{
						for(VirtualFile child : file.getChildren())
						{
							propagateEvent(child, callback, propagateViaCachedChildren);
						}
					}
				}
				else
				{
					try
					{
						callback.invoke(JavaScriptIndex.this, file);
					}
					catch(Exception e)
					{
						throw new RuntimeException(e);
					}
				}
			}

			public void beforeFileDeletion(VirtualFileEvent event)
			{
				if(this != myFileListener)
				{
					return; // disposed
				}
				final VirtualFile fileOrDir = event.getFile();
				propagateEvent(fileOrDir, fileRemovedCallback, true);
			}

			public void beforeContentsChange(final VirtualFileEvent event)
			{
				if(this != myFileListener)
				{
					return; // disposed
				}
				propagateEvent(event.getFile(), fileChangedCallback, false);
			}

			public void beforePropertyChange(final VirtualFilePropertyEvent event)
			{
				if(this != myFileListener)
				{
					return; // disposed
				}
				if(VirtualFile.PROP_NAME.equals(event.getPropertyName()))
				{
					propagateEvent(event.getFile(), fileRemovedCallback, false);
				}
			}

			public void propertyChanged(final VirtualFilePropertyEvent event)
			{
				if(this != myFileListener)
				{
					return; // disposed
				}
				if(VirtualFile.PROP_NAME.equals(event.getPropertyName()))
				{
					propagateEvent(event.getFile(), fileAddedCallback, false);
				}
			}

			public void beforeFileMovement(final VirtualFileMoveEvent event)
			{
				if(this != myFileListener)
				{
					return; // disposed
				}
				propagateEvent(event.getFile(), fileRemovedCallback, false);
			}

			public void fileMoved(final VirtualFileMoveEvent event)
			{
				if(this != myFileListener)
				{
					return; // disposed
				}
				propagateEvent(event.getFile(), fileAddedCallback, false);
			}
		};

		myConnection = myProject.getMessageBus().connect();

		VirtualFileManager.getInstance().addVirtualFileListener(myFileListener);

		myConnection.subscribe(ProjectTopics.PROJECT_ROOTS, new ModuleRootListener()
		{
			public void beforeRootsChange(ModuleRootEvent event)
			{
			}

			public void rootsChanged(ModuleRootEvent event)
			{
				requestUpdateCaches();
			}
		});
	}

	private boolean isAcceptableFile(final VirtualFile fileOrDir)
	{
		return myFileTypeManager.getFileTypeByFile(fileOrDir) == JavaScriptSupportLoader.JAVASCRIPT;
	}

	private void requestUpdateCaches()
	{
		Runnable runnable = new Runnable()
		{
			public void run()
			{
				if(myProject.isDisposed())
				{
					return;
				}

				myOldJavaScriptFiles = myJavaScriptFiles;
				myJavaScriptFiles = new THashMap<String, JSIndexEntry>(myOldJavaScriptFiles != null ? myOldJavaScriptFiles.size() : 10);

				if(ApplicationManager.getApplication().isCommandLine())
				{
					myUpdateRunnable.run();
				}
				else
				{
					ProgressManager.getInstance().runProcessWithProgressSynchronously(myUpdateRunnable, JSBundle.message("building.index.message"), false,
							myProject);
				}

				if(myOldJavaScriptFiles != null)
				{
					for(JSIndexEntry entry : myOldJavaScriptFiles.values())
					{
						entry.invalidate();
					}
					myOldJavaScriptFiles = null;
				}
			}
		};

		if(ApplicationManager.getApplication().isHeadlessEnvironment() && !ApplicationManager.getApplication().isUnitTestMode())
		{
			runnable.run();
		}
		else
		{
			ApplicationManager.getApplication().invokeLater(runnable, ModalityState.NON_MODAL);
		}
	}

	private static void processModulesThatContainExternalJSDefinitions(@NotNull Processor<VirtualFile> processor, @NotNull Project project)
	{
		Set<VirtualFile> processed = new java.util.HashSet<VirtualFile>();

		for(Module module : ModuleManager.getInstance(project).getModules())
		{
			final VirtualFile file = getFlexSdkLocation(module);
			if(file == null)
			{
				continue;
			}

			if(!processed.contains(file))
			{ // index source files
				processed.add(file);
				processor.process(file);
			}
		}
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

	private void initPredefines(ProgressIndicator progress)
	{
		for(String name : ourPredefinedFileNames)
		{
			initPredefinedFile(progress, name);
		}
	}

	private void initPredefinedFile(final ProgressIndicator progress, final String name)
	{
		PsiFile file;
		final JSIndexEntry indexEntry = myPredefinedJavaScriptFiles.get(name);

		if(indexEntry == null)
		{
			if(progress != null)
			{
				progress.setText2(name);
			}
			file = createPredefinesFromModel(name);
		}
		else
		{
			file = indexEntry.getFile();
		}
	}

	public synchronized void loadCaches(final ProgressIndicator progress)
	{
		DataInputStream input = null;
		final File cacheFile = getCacheLocation(JS_CACHES_DIR_NAME);

		boolean rebuildCachesRequested = true;
		boolean loadingCachesStarted = false;

		try
		{
			if(!cacheFile.exists())
			{
				return; // should be in try to ensure 'predefines' initialized
			}

			input = new DataInputStream(new BufferedInputStream(new FileInputStream(cacheFile)));
			int version = input.readByte();
			if(version != CURRENT_VERSION)
			{
				return;
			}

			if(progress != null)
			{
				progress.pushState();
				progress.setText(JSBundle.message("loading.index.message"));
				loadingCachesStarted = true;
			}

			final int fileCount = input.readInt();
			final PsiManager manager = PsiManager.getInstance(myProject);
			final int namesCount = input.readInt();
			DeserializationContext context = new DeserializationContext(input, manager, myIndex2Names);

			myIndex2Names.ensureCapacity(namesCount);
			myNames2Index.ensureCapacity(namesCount);

			for(int i = 0; i < namesCount; ++i)
			{
				final String name = input.readUTF();
				final int index = input.readInt();
				myIndex2Names.put(index, name);
				myNames2Index.put(name, index);
			}

			myRootPackage.deserialize(context);

			List<JSIndexEntry> indexEntriesToInvalidate = null;

			for(int i = 0; i < fileCount; i++)
			{
				final String url = input.readUTF();
				final long stamp = input.readLong();

				if(progress != null)
				{
					progress.setText2(url);
					progress.setFraction(((double) i) / fileCount);
				}

				boolean predefined = ourPredefinedFileNames.contains(url);
				boolean outdated = false;
				final JSIndexEntry value;

				if(!predefined)
				{
					VirtualFile relativeFile = UriUtil.findRelativeFile(url, null);

					if(relativeFile == null || stamp != relativeFile.getTimeStamp())
					{
						outdated = true;
					}

					PsiFile psiFile = relativeFile != null ? manager.findFile(relativeFile) : null;
					if(!(psiFile instanceof PsiFile))
					{
						outdated = true;
					}

					if(relativeFile != null && !outdated)
					{
						Module module = ProjectRootManager.getInstance(myProject).getFileIndex().getModuleForFile(relativeFile);

						if(module == null)
						{
							boolean ancestorOfUsedJdk = false;

							for(VirtualFile file : setOfExternalDefsRoots)
							{
								if(VfsUtil.isAncestor(file, relativeFile, true))
								{
									ancestorOfUsedJdk = true;
								}
							}

							outdated = !ancestorOfUsedJdk;
						}
					}

					final VirtualFile effectiveVirtualFile = outdated ? null : relativeFile;
					value = new JSIndexEntry(context, effectiveVirtualFile);

				}
				else
				{
					value = new PredefinedJSIndexEntry(context, url);
				}

				if(!outdated)
				{
					((predefined) ? myPredefinedJavaScriptFiles : myJavaScriptFiles).put(url, value);
				}
				else
				{
					if(indexEntriesToInvalidate == null)
					{
						indexEntriesToInvalidate = new ArrayList<JSIndexEntry>(5);
					}
					indexEntriesToInvalidate.add(value); // we delay invalidation since it might cause package structure corruption
				}
			}

			if(indexEntriesToInvalidate != null)
			{
				for(JSIndexEntry entry : indexEntriesToInvalidate)
				{
					entry.invalidate(myProject);
				}
			}
			rebuildCachesRequested = false;
		}
		catch(IOException e)
		{

		}
		finally
		{
			if(input != null)
			{
				try
				{
					input.close();
				}
				catch(IOException e1)
				{
				}
			}

			if(rebuildCachesRequested)
			{
				cacheFile.delete();
				clear();
			}

			if(progress != null && loadingCachesStarted)
			{
				progress.popState();
			}

			initPredefines(progress);
		}
	}

	private File getCacheLocation(final String dirName)
	{
		final String cacheFileName = myProject.getName() + "." + myProject.getLocationHash();
		return new File(PathManager.getSystemPath() + File.separator + dirName + File.separator + cacheFileName);
	}

	public synchronized void saveCaches()
	{
		final File cacheFile = getCacheLocation(JS_CACHES_DIR_NAME);
		FileUtil.createParentDirs(cacheFile);
		DataOutputStream output = null;

		try
		{
			output = new DataOutputStream(new BufferedOutputStream(new FileOutputStream(cacheFile)));
			SerializationContext context = new SerializationContext(output, myProject, myJavaScriptFiles.size());

			enumerateNames(myPredefinedJavaScriptFiles, context);
			enumerateNames(myJavaScriptFiles, context);
			myRootPackage.enumerateNames(context); // we need to enumerate packages AFTER passing all entries since revalidation of old files cause package
			// update

			output.writeByte(CURRENT_VERSION);
			output.writeInt(context.getFilesCount() + myPredefinedJavaScriptFiles.size());

			int namesCount = context.myNames.size();
			output.writeInt(namesCount);

			TObjectIntIterator<String> iterator = context.myNames.iterator();
			while(iterator.hasNext())
			{
				iterator.advance();
				output.writeUTF(iterator.key());
				output.writeInt(iterator.value());
				--namesCount;
			}

			assert namesCount == 0;

			myRootPackage.serialize(context);
			writeEntries(myPredefinedJavaScriptFiles, output, context);
			writeEntries(myJavaScriptFiles, output, context);
			output.close();
		}
		catch(IOException e)
		{
			LOG.debug(e);
			if(output != null)
			{
				try
				{
					output.close();
					output = null;
				}
				catch(IOException e1)
				{
				}
			}
			cacheFile.delete();
		}
		finally
		{
			if(output != null)
			{
				try
				{
					output.close();
				}
				catch(IOException e1)
				{
				}
			}
		}
	}

	private static void writeEntries(final Map<String, JSIndexEntry> entries, final DataOutputStream output,
			final SerializationContext context) throws IOException
	{
		for(final String key : entries.keySet())
		{
			final JSIndexEntry value = entries.get(key);
			if(!value.isUpToDate())
			{
				continue;
			}

			output.writeUTF(key);
			output.writeLong(value.getTimeStamp());

			value.write(context);
		}
	}

	private void enumerateNames(final Map<String, JSIndexEntry> entries, final SerializationContext context)
	{
		for(Iterator<JSIndexEntry> i = entries.values().iterator(); i.hasNext(); )
		{
			JSIndexEntry value = i.next();

			if(!value.isUpToDate())
			{
				value.invalidate();
				myFilesToUpdate.remove(value);
				context.decFileCount();
				i.remove();
				continue;
			}
			value.enumerateNames(context);
		}
	}

	private PsiFile createPredefinesFromModel(final String fileName)
	{
		final JSIndexEntry value = new PredefinedJSIndexEntry(fileName, myProject, false);
		myPredefinedJavaScriptFiles.put(fileName, value);
		value.initTypesAndBrowserSpecifics();
		return value.getFile();
	}

	private void fileAdded(final VirtualFile fileOrDir)
	{
		if(isAcceptableFile(fileOrDir) && !myProject.isDisposed())
		{
			processFileAdded(fileOrDir);
		}
	}

	// invoked via reflection
	private void fileRemoved(final VirtualFile fileOrDir)
	{
		if(isAcceptableFile(fileOrDir))
		{
			if(!myProject.isDisposed())
			{
				processFileRemoved(fileOrDir);
			}
		}
	}

	// invoked via reflection
	private void fileChanged(final VirtualFile fileOrDir)
	{
		if(isAcceptableFile(fileOrDir) && !myProject.isDisposed())
		{
			processFileChanged(fileOrDir);
		}
	}

	public void projectClosed()
	{
		if(myFileListener != null)
		{
			VirtualFileManager.getInstance().removeVirtualFileListener(myFileListener);
			PsiManager.getInstance(myProject).removePsiTreeChangeListener(myTreeChangeListener);

			myConnection.disconnect();

			if(!ApplicationManager.getApplication().isCommandLine())
			{
				saveCaches();
			}

			myFileListener = null;
		}
		clear();
	}

	public synchronized void processFileAdded(final VirtualFile file)
	{
		final String url = file.getPath();

		if(myOldJavaScriptFiles != null)
		{
			final JSIndexEntry jsIndexEntry = myOldJavaScriptFiles.get(url);

			if(jsIndexEntry != null)
			{
				myJavaScriptFiles.put(url, jsIndexEntry);
				myOldJavaScriptFiles.remove(url);
				return;
			}
		}

		final ProgressIndicator progress = ProgressManager.getInstance().getProgressIndicator();
		if(progress != null)
		{
			progress.setText2(file.getPresentableUrl());
		}

		if(!myJavaScriptFiles.contains(url))
		{
			try
			{
				final JSIndexEntry value = new JSIndexEntry(file, myProject, !myDoingFilesRescan, null);
				final JSIndexEntry indexEntry = myJavaScriptFiles.put(url, value);  // psi file event child (file) created after virtual file event file created
				// might cause duplication of index entries
				if(!myDoingFilesRescan && indexEntry == null)
				{
					myFilesToUpdate.add(value);
				}
			}
			catch(AssertionError ae)
			{
				// could not retrieve psi file for some reason
				LOG.error(ae);
			}
		}
	}

	private synchronized void processFileChanged(final VirtualFile file)
	{
		final JSIndexEntry indexEntry = myJavaScriptFiles.get(file.getPath());

		if(indexEntry == null)
		{
			processFileAdded(file);
		}
		else
		{
			myFilesToUpdate.add(indexEntry);
		}
	}

	private synchronized void processFileRemoved(final @NotNull VirtualFile file)
	{
		final JSIndexEntry jsIndexEntry = myJavaScriptFiles.remove(file.getPath());
		if(jsIndexEntry != null)
		{
			jsIndexEntry.invalidate();
			myFilesToUpdate.remove(jsIndexEntry);
		}
	}

	@NonNls
	public String getComponentName()
	{
		return "JavaScriptIndex";
	}

	public void initComponent()
	{
	}

	public void disposeComponent()
	{
		if(myFileListener != null)
		{
			projectClosed();
		}
	}

	public static JavaScriptIndex getInstance(final Project project)
	{
		return project.getComponent(JavaScriptIndex.class);
	}

	public synchronized void clear()
	{
		myJavaScriptFiles.clear();
		myPredefinedJavaScriptFiles.clear();
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

		processPredefinedEntries(processor, ecmaL4);

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

	public synchronized void processPredefinedEntries(JavaScriptSymbolProcessor processor, boolean ecmaL4)
	{
		for(Map.Entry<String, JSIndexEntry> entry : myPredefinedJavaScriptFiles.entrySet())
		{
			if(ecmaL4 && entry.getValue().getFile().getLanguage() != JavaScriptSupportLoader.ECMA_SCRIPT_L4)
			{
				continue;
			}
			entry.getValue().processSymbolsNoLock(processor);
		}
	}

	private void collectFilesUnderDirectory(final VirtualFile homeDirectory, final Collection<VirtualFile> additionalFiles,
			final boolean searchForSourceDir)
	{
		if(searchForSourceDir)
		{
			if(homeDirectory.isDirectory())
			{
				for(VirtualFile f : homeDirectory.getChildren())
				{
					if(f.isDirectory())
					{
						final @NonNls String nameWOExtension = f.getNameWithoutExtension();
						if("source".equals(nameWOExtension) ||
								"src".equals(nameWOExtension) ||
								"libs".equals(nameWOExtension))
						{
							collectFilesUnderDirectory(f, additionalFiles, false);
						}
						else
						{
							collectFilesUnderDirectory(f, additionalFiles, true);
						}
					}
				}
			}
			else if(isAcceptableFile(homeDirectory) && myJavaScriptFiles.get(homeDirectory.getPath()) == null)
			{
				additionalFiles.add(homeDirectory);
			}
		}
		else
		{
			if(homeDirectory.isDirectory())
			{
				for(VirtualFile f : homeDirectory.getChildren())
				{
					if(f.isDirectory())
					{
						collectFilesUnderDirectory(f, additionalFiles, false);
					}
					else if((isAcceptableFile(f) && myJavaScriptFiles.get(f.getPath()) == null))
					{
						additionalFiles.add(f);
					}
				}
			}
			else if(isAcceptableFile(homeDirectory) && myJavaScriptFiles.get(homeDirectory.getPath()) == null)
			{
				additionalFiles.add(homeDirectory);
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
			String s = fileName;
			if(s.startsWith(PREDEFINES_PREFIX))
			{
				s = fileName.substring(PREDEFINES_PREFIX.length());
				s = s.substring(0, s.length() - PREDEFINED_SCRIPTS_FILE_EXTENSION.length() + 1) + "xml";
			}
			indexEntry = myPredefinedJavaScriptFiles.get(s);
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
			public PsiFile getBaseFile()
			{
				return indexEntry.getFile();
			}

			public int getRequiredNameId()
			{
				return nameId;
			}

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

	public Set<VirtualFile> getECMAScriptFilesSetFromEntries()
	{
		if(myPredefinedJavaScriptVirtualFiles.isEmpty())
		{
			for(String s : ourPredefinedFileNames)
			{
				if(s.endsWith(".xml"))
				{
					continue;
				}
				addOneFile(s);
			}
			addOneFile("ECMAScript_Additional.js2");
		}
		return myPredefinedJavaScriptVirtualFiles;
	}

	private void addOneFile(String s)
	{
		URL resource = Marker.class.getResource(s);
		VirtualFile path = VfsUtil.findFileByURL(resource);
		assert path != null;
		myPredefinedJavaScriptVirtualFiles.add(path);
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

	private class JSTreeChangeListener extends PsiTreeChangeAdapter
	{
		public void childAdded(PsiTreeChangeEvent event)
		{
			final PsiElement child = event.getChild();
			if(child instanceof JSFile && child.isPhysical())
			{
				processFileAdded(((JSFile) child).getVirtualFile());
			}
			else
			{
				process(event);
			}
		}

		public void childrenChanged(PsiTreeChangeEvent event)
		{
			process(event);
		}

		public void childRemoved(PsiTreeChangeEvent event)
		{
			if(event.getChild() instanceof JSFile)
			{
				processFileRemoved(((JSFile) event.getChild()).getVirtualFile());
			}
			else
			{
				process(event);
			}
		}

		public void childReplaced(PsiTreeChangeEvent event)
		{
			process(event);
		}

		private void process(final PsiTreeChangeEvent event)
		{
			final PsiElement psiElement = event.getParent();

			if(psiElement != null && psiElement.isValid())
			{
				final PsiFile psiFile = psiElement.getContainingFile();

				if(psiFile instanceof JSFile && psiFile.isPhysical() && isAcceptableFile(psiFile.getVirtualFile()))
				{
					final VirtualFile file = psiFile.getVirtualFile();
					if(file.isValid())
					{
						processFileChanged(file);
					}
				}
			}
		}
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
