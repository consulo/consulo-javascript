package com.intellij.lang.javascript.psi.impl;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtil;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.roots.LibraryOrderEntry;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.roots.OrderEntry;
import com.intellij.openapi.roots.OrderRootType;
import com.intellij.openapi.roots.ProjectFileIndex;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.roots.SdkOrderEntry;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.LocalFileSystem;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.openapi.vfs.newvfs.ManagingFS;
import com.intellij.psi.PsiDirectory;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiFileSystemItem;
import com.intellij.psi.PsiLanguageInjectionHost;
import com.intellij.psi.PsiManager;
import com.intellij.psi.PsiReference;
import com.intellij.psi.impl.source.resolve.reference.impl.providers.FileReferenceSet;
import com.intellij.util.Function;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: 08.04.2009
 * Time: 20:48:38
 * To change this template use File | Settings | File Templates.
 */
public class ReferenceSupport {

  public static PsiReference[] getFileRefs(final PsiElement elt,
                                           final PsiElement valueNode,
                                           final int offset,
                                           final LookupOptions lookupOptions) {
    String str = StringUtil.stripQuotesAroundValue(valueNode.getText());
    return getFileRefs(elt, offset, str, lookupOptions);
  }

  public static PsiReference[] getFileRefs(final PsiElement elt, final int offset, String str,
                                           final LookupOptions lookupOptions) {

    if (lookupOptions.IGNORE_TEXT_ARTER_HASH) {
      int hashIndex = str.indexOf('#');
      if (hashIndex != -1) str = str.substring(0, hashIndex);
    }

    final FileReferenceSet base = new FileReferenceSet(str, elt, offset, null, SystemInfo.isFileSystemCaseSensitive);

    final boolean lookForAbsolutePath = lookupOptions.ABSOLUTE && new File(str).isAbsolute();
    final boolean startsWithSlash = str.startsWith("/");

    base.addCustomization(FileReferenceSet.DEFAULT_PATH_EVALUATOR_OPTION, new Function<PsiFile, Collection<PsiFileSystemItem>>() {
      public Collection<PsiFileSystemItem> fun(PsiFile psiFile) {
        final PsiElement context = psiFile.getContext();
        if (context instanceof PsiLanguageInjectionHost) {
          psiFile = context.getContainingFile();
        }
        PsiFile originalFile = psiFile.getOriginalFile();
        if (originalFile != null) psiFile = originalFile;

        final List<VirtualFile> dirs = new ArrayList<VirtualFile>();

        // paths relative to file should not start with slash
        if (lookupOptions.RELATIVE_TO_FILE && !startsWithSlash) {
          appendFileLocation(dirs, psiFile);
        }

        if ((lookupOptions.RELATIVE_TO_SOURCE_ROOTS_START_WITH_SLASH && startsWithSlash) ||
            (lookupOptions.RELATIVE_TO_SOURCE_ROOTS_START_WITHOUT_SLASH && !startsWithSlash)) {
          appendSourceRoots(dirs, psiFile);
        }

        if (lookForAbsolutePath) {
          appendFileSystemRoot(dirs, psiFile.getProject());
        }

        if (lookupOptions.RELATIVE_TO_PROJECT_BASE_DIR) {
          dirs.add(psiFile.getProject().getBaseDir());
        }

        if (lookupOptions.IN_SDK_AND_LIBRARY_CLASS_ROOTS) {
          appendSdkAndLibraryClassRoots(dirs, psiFile);
        }

        final Collection<PsiFileSystemItem> result = new ArrayList<PsiFileSystemItem>();
        final PsiManager psiManager = PsiManager.getInstance(psiFile.getProject());
        for (final VirtualFile dir : dirs) {
          if (dir != null) {
            final PsiDirectory psiDir = psiManager.findDirectory(dir);
            if (psiDir != null) {
              result.add(psiDir);
            }
          }
        }
        return result;
      }
    });
    return base.getAllReferences();
  }

  private static void appendFileLocation(final List<VirtualFile> dirs, final PsiFile psiFile) {
    final VirtualFile file = psiFile.getVirtualFile();
    if (file != null) {
      dirs.add(file.getParent());
    }
  }

  private static void appendSourceRoots(final Collection<VirtualFile> dirs, final PsiFile psiFile) {
    final VirtualFile file = psiFile.getVirtualFile();
    if (file == null) return;
    final Project project = psiFile.getProject();
    final ProjectFileIndex index = ProjectRootManager.getInstance(project).getFileIndex();
    final Module module = index.getModuleForFile(file);
    if (module != null && index.getSourceRootForFile(file) != null) {
      dirs.addAll(Arrays.asList(ModuleRootManager.getInstance(module).getSourceRoots()));
    }
  }

  private static void appendFileSystemRoot(final Collection<VirtualFile> dirs, final Project project) {
    final VirtualFile fileSystemRoot;
    if (SystemInfo.isWindows) {
      fileSystemRoot = ManagingFS.getInstance().findRoot("", LocalFileSystem.getInstance());
    }
    else {
      fileSystemRoot = LocalFileSystem.getInstance().findFileByPath("/");
    }
    dirs.add(fileSystemRoot);
  }

  private static void appendSdkAndLibraryClassRoots(List<VirtualFile> dirs, PsiFile psiFile) {
    final Module module = ModuleUtil.findModuleForPsiElement(psiFile);
    if (module != null) {
      final OrderEntry[] orderEntries = ModuleRootManager.getInstance(module).getOrderEntries();
      for (final OrderEntry orderEntry : orderEntries) {
        if (orderEntry instanceof LibraryOrderEntry || orderEntry instanceof SdkOrderEntry) {
          dirs.addAll(Arrays.asList(orderEntry.getFiles(OrderRootType.CLASSES)));
        }
      }
    }
  }

  public static class LookupOptions {

    // default is absolute or relative to current file
    public static final LookupOptions DEFAULT = new LookupOptions(false, true, true, false, false, false, false);
    public static final LookupOptions MX_STYLE_SOURCE = new LookupOptions(false, true, true, true, true, false, true);
    public static final LookupOptions EMBEDDED_ASSET = new LookupOptions(true, true, true, true, true, false, true);
    public static final LookupOptions NON_EMBEDDED_ASSET = new LookupOptions(false, true, false, false, true, false, false);
    public static final LookupOptions FLEX_COMPILER_CONFIG_PATH_ELEMENT = new LookupOptions(false, true, true, false, false, true, false);

    public final boolean IGNORE_TEXT_ARTER_HASH;
    public final boolean ABSOLUTE;
    public final boolean RELATIVE_TO_FILE;
    public final boolean RELATIVE_TO_SOURCE_ROOTS_START_WITH_SLASH;
    public final boolean RELATIVE_TO_SOURCE_ROOTS_START_WITHOUT_SLASH;
    public final boolean RELATIVE_TO_PROJECT_BASE_DIR; // better name would be RELATIVE_TO_FCSH_START_DIR but FlexUtils.getFlexCompilerStartDirectory() is not accessible from this class
    public final boolean IN_SDK_AND_LIBRARY_CLASS_ROOTS;

    public LookupOptions(boolean IGNORE_TEXT_ARTER_HASH,
                         boolean ABSOLUTE,
                         boolean RELATIVE_TO_FILE,
                         boolean RELATIVE_TO_SOURCE_ROOTS_START_WITH_SLASH,
                         boolean RELATIVE_TO_SOURCE_ROOTS_START_WITHOUT_SLASH,
                         boolean RELATIVE_TO_PROJECT_BASE_DIR,
                         boolean IN_SDK_AND_LIBRARY_CLASS_ROOTS) {
      this.IGNORE_TEXT_ARTER_HASH = IGNORE_TEXT_ARTER_HASH;
      this.ABSOLUTE = ABSOLUTE;
      this.RELATIVE_TO_FILE = RELATIVE_TO_FILE;
      this.RELATIVE_TO_SOURCE_ROOTS_START_WITH_SLASH = RELATIVE_TO_SOURCE_ROOTS_START_WITH_SLASH;
      this.RELATIVE_TO_SOURCE_ROOTS_START_WITHOUT_SLASH = RELATIVE_TO_SOURCE_ROOTS_START_WITHOUT_SLASH;
      this.RELATIVE_TO_PROJECT_BASE_DIR = RELATIVE_TO_PROJECT_BASE_DIR;
      this.IN_SDK_AND_LIBRARY_CLASS_ROOTS = IN_SDK_AND_LIBRARY_CLASS_ROOTS;
    }
  }
}
