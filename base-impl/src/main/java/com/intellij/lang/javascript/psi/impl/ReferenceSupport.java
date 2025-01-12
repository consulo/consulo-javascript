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

package com.intellij.lang.javascript.psi.impl;

import consulo.annotation.access.RequiredReadAction;
import consulo.application.util.SystemInfo;
import consulo.content.base.BinariesOrderRootType;
import consulo.language.psi.*;
import consulo.language.psi.path.FileReferenceSet;
import consulo.language.util.ModuleUtilCore;
import consulo.module.Module;
import consulo.module.content.ModuleRootManager;
import consulo.module.content.ProjectFileIndex;
import consulo.module.content.ProjectRootManager;
import consulo.module.content.layer.orderEntry.OrderEntry;
import consulo.module.content.layer.orderEntry.OrderEntryWithTracking;
import consulo.platform.Platform;
import consulo.project.Project;
import consulo.util.lang.StringUtil;
import consulo.virtualFileSystem.LocalFileSystem;
import consulo.virtualFileSystem.ManagingFS;
import consulo.virtualFileSystem.VirtualFile;

import java.io.File;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.List;

/**
 * @author Maxim.Mossienko
 * @since 2009-04-08
 */
public class ReferenceSupport {
    @RequiredReadAction
    public static PsiReference[] getFileRefs(PsiElement elt, PsiElement valueNode, int offset, LookupOptions lookupOptions) {
        String str = StringUtil.stripQuotesAroundValue(valueNode.getText());
        return getFileRefs(elt, offset, str, lookupOptions);
    }

    public static PsiReference[] getFileRefs(PsiElement elt, int offset, String str, LookupOptions lookupOptions) {
        if (lookupOptions.IGNORE_TEXT_ARTER_HASH) {
            int hashIndex = str.indexOf('#');
            if (hashIndex != -1) {
                str = str.substring(0, hashIndex);
            }
        }

        FileReferenceSet base = new FileReferenceSet(str, elt, offset, null, SystemInfo.isFileSystemCaseSensitive);

        boolean lookForAbsolutePath = lookupOptions.ABSOLUTE && new File(str).isAbsolute();
        boolean startsWithSlash = str.startsWith("/");

        base.addCustomization(
            FileReferenceSet.DEFAULT_PATH_EVALUATOR_OPTION,
            psiFile -> {
                PsiElement context = psiFile.getContext();
                if (context instanceof PsiLanguageInjectionHost languageInjectionHost) {
                    psiFile = languageInjectionHost.getContainingFile();
                }
                PsiFile originalFile = psiFile.getOriginalFile();
                if (originalFile != null) {
                    psiFile = originalFile;
                }

                List<VirtualFile> dirs = new ArrayList<>();

                // paths relative to file should not start with slash
                if (lookupOptions.RELATIVE_TO_FILE && !startsWithSlash) {
                    appendFileLocation(dirs, psiFile);
                }

                if ((lookupOptions.RELATIVE_TO_SOURCE_ROOTS_START_WITH_SLASH && startsWithSlash)
                    || (lookupOptions.RELATIVE_TO_SOURCE_ROOTS_START_WITHOUT_SLASH && !startsWithSlash)) {
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

                Collection<PsiFileSystemItem> result = new ArrayList<>();
                PsiManager psiManager = PsiManager.getInstance(psiFile.getProject());
                for (VirtualFile dir : dirs) {
                    if (dir != null) {
                        PsiDirectory psiDir = psiManager.findDirectory(dir);
                        if (psiDir != null) {
                            result.add(psiDir);
                        }
                    }
                }
                return result;
            }
        );
        return base.getAllReferences();
    }

    private static void appendFileLocation(List<VirtualFile> dirs, PsiFile psiFile) {
        VirtualFile file = psiFile.getVirtualFile();
        if (file != null) {
            dirs.add(file.getParent());
        }
    }

    private static void appendSourceRoots(Collection<VirtualFile> dirs, PsiFile psiFile) {
        VirtualFile file = psiFile.getVirtualFile();
        if (file == null) {
            return;
        }
        Project project = psiFile.getProject();
        ProjectFileIndex index = ProjectRootManager.getInstance(project).getFileIndex();
        Module module = index.getModuleForFile(file);
        if (module != null && index.getSourceRootForFile(file) != null) {
            dirs.addAll(Arrays.asList(ModuleRootManager.getInstance(module).getSourceRoots()));
        }
    }

    private static void appendFileSystemRoot(Collection<VirtualFile> dirs, Project project) {
        VirtualFile fileSystemRoot;
        if (Platform.current().os().isWindows()) {
            fileSystemRoot = ManagingFS.getInstance().findRoot("", LocalFileSystem.getInstance());
        }
        else {
            fileSystemRoot = LocalFileSystem.getInstance().findFileByPath("/");
        }
        dirs.add(fileSystemRoot);
    }

    @RequiredReadAction
    private static void appendSdkAndLibraryClassRoots(List<VirtualFile> dirs, PsiFile psiFile) {
        Module module = ModuleUtilCore.findModuleForPsiElement(psiFile);
        if (module != null) {
            OrderEntry[] orderEntries = ModuleRootManager.getInstance(module).getOrderEntries();
            for (OrderEntry orderEntry : orderEntries) {
                if (orderEntry instanceof OrderEntryWithTracking) {
                    dirs.addAll(Arrays.asList(orderEntry.getFiles(BinariesOrderRootType.getInstance())));
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
        public static final LookupOptions FLEX_COMPILER_CONFIG_PATH_ELEMENT =
            new LookupOptions(false, true, true, false, false, true, false);

        public final boolean IGNORE_TEXT_ARTER_HASH;
        public final boolean ABSOLUTE;
        public final boolean RELATIVE_TO_FILE;
        public final boolean RELATIVE_TO_SOURCE_ROOTS_START_WITH_SLASH;
        public final boolean RELATIVE_TO_SOURCE_ROOTS_START_WITHOUT_SLASH;
        public final boolean RELATIVE_TO_PROJECT_BASE_DIR;
        // better name would be RELATIVE_TO_FCSH_START_DIR but FlexUtils.getFlexCompilerStartDirectory
        // () is not accessible from this class
        public final boolean IN_SDK_AND_LIBRARY_CLASS_ROOTS;

        public LookupOptions(
            boolean IGNORE_TEXT_ARTER_HASH,
            boolean ABSOLUTE,
            boolean RELATIVE_TO_FILE,
            boolean RELATIVE_TO_SOURCE_ROOTS_START_WITH_SLASH,
            boolean RELATIVE_TO_SOURCE_ROOTS_START_WITHOUT_SLASH,
            boolean RELATIVE_TO_PROJECT_BASE_DIR,
            boolean IN_SDK_AND_LIBRARY_CLASS_ROOTS
        ) {
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
