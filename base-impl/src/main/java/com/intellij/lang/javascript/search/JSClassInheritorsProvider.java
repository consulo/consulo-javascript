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

package com.intellij.lang.javascript.search;

import java.util.Collection;

import com.intellij.lang.javascript.psi.JSClass;
import consulo.component.extension.ExtensionPointName;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.project.Project;

public interface JSClassInheritorsProvider
{
	ExtensionPointName<JSClassInheritorsProvider> EP_NAME = ExtensionPointName.create("consulo.javascript.classInheritorsProvider");

	Collection<JSClass> getImplementingClasses(String parentName, Project project, GlobalSearchScope scope);

	Collection<JSClass> getExtendingClasses(String parentName, Project project, GlobalSearchScope scope);
}
