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

package consulo.json.validation.descriptionByAnotherPsiElement;

import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.json.validation.descriptor.JsonObjectDescriptor;
import consulo.ui.annotation.RequiredUIAccess;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 12.11.2015
 */
public interface DescriptionByAnotherPsiElementProvider<T extends PsiElement>
{
	ExtensionPointName<DescriptionByAnotherPsiElementProvider<?>> EP_NAME = ExtensionPointName.create("consulo.javascript.jsonDescriptionByAnotherPsiElementProvider");

	@Nonnull
	String getId();

	@Nonnull
	String getPsiElementName();

	@Nonnull
	@RequiredReadAction
	String getIdFromPsiElement(@Nonnull T element);

	@Nullable
	@RequiredReadAction
	T getPsiElementById(@Nonnull String id, @Nonnull Project project);

	@RequiredUIAccess
	@Nullable
	T chooseElement(@Nonnull Project project);

	@RequiredReadAction
	boolean isAvailable(@Nonnull Project project);

	void fillRootObject(@Nonnull T psiElement, @Nonnull JsonObjectDescriptor root);
}
