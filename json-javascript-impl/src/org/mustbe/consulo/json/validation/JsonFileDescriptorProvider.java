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

package org.mustbe.consulo.json.validation;

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import org.mustbe.consulo.json.validation.descriptor.JsonObjectDescriptor;
import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.psi.PsiFile;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public interface JsonFileDescriptorProvider
{
	ExtensionPointName<JsonFileDescriptorProvider> EP_NAME = ExtensionPointName.create("org.mustbe.consulo.javascript.jsonFileDescriptorProvider");

	@RequiredReadAction
	boolean isMyFile(@NotNull PsiFile file);

	@RequiredReadAction
	void fillRootObject(@NotNull JsonObjectDescriptor root, @NotNull PsiFile file);
}
