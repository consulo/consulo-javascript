/*
 * Copyright 2013-2015 must-be.org
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

package org.mustbe.consulo.json.validation.descriptionByAnotherPsiElement;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredDispatchThread;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.json.validation.descriptor.JsonObjectDescriptor;
import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;

/**
 * @author VISTALL
 * @since 12.11.2015
 */
public interface DescriptionByAnotherPsiElementProvider<T extends PsiElement>
{
	ExtensionPointName<DescriptionByAnotherPsiElementProvider<?>> EP_NAME = ExtensionPointName.create("org.mustbe.consulo.javascript.jsonDescriptionByAnotherPsiElementProvider");

	@NotNull
	String getId();

	@NotNull
	String getPsiElementName();

	@NotNull
	@RequiredReadAction
	String getIdFromPsiElement(@NotNull T element);

	@Nullable
	@RequiredReadAction
	T getPsiElementById(@NotNull String id, @NotNull Project project);

	@RequiredDispatchThread
	@Nullable
	T chooseElement(@NotNull Project project);

	@RequiredReadAction
	boolean isAvailable(@NotNull Project project);

	void fillRootObject(@NotNull T psiElement, @NotNull JsonObjectDescriptor root);
}
