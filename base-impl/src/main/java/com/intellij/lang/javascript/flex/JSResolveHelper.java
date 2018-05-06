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

import javax.annotation.Nullable;

import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.util.Processor;

/**
 * @author yole
 */
public interface JSResolveHelper
{
	ExtensionPointName<JSResolveHelper> EP_NAME = ExtensionPointName.create("consulo.javascript.resolveHelper");

	// TODO: drop module
	@Nullable
	PsiElement findClassByQName(final String link, final Project project, final String className, GlobalSearchScope scope);

	void importClass(final PsiScopeProcessor processor, final PsiNamedElement file, final String packageQualifierText);

	boolean processPackage(final String packageQualifierText, String resolvedName, final Processor<VirtualFile> processor,
			GlobalSearchScope globalSearchScope, Project project);
}
