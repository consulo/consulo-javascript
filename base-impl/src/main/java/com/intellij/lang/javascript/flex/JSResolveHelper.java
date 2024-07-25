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

package com.intellij.lang.javascript.flex;

import consulo.annotation.component.ComponentScope;
import consulo.annotation.component.ExtensionAPI;
import consulo.application.util.function.Processor;
import consulo.component.extension.ExtensionPointName;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.annotation.DeprecationInfo;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.project.Project;
import consulo.virtualFileSystem.VirtualFile;

import jakarta.annotation.Nullable;

/**
 * @author yole
 */
@Deprecated
@DeprecationInfo("We need this?")
@ExtensionAPI(ComponentScope.APPLICATION)
public interface JSResolveHelper {
    ExtensionPointName<JSResolveHelper> EP_NAME = ExtensionPointName.create(JSResolveHelper.class);

    // TODO: drop module
    @Nullable
    PsiElement findClassByQName(final String link, final Project project, final String className, GlobalSearchScope scope);

    void importClass(final PsiScopeProcessor processor, final PsiNamedElement file, final String packageQualifierText);

    boolean processPackage(
        final String packageQualifierText,
		String resolvedName,
		final Processor<VirtualFile> processor,
        GlobalSearchScope globalSearchScope,
		Project project
    );
}
