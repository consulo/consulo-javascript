/*
 * Copyright 2000-2005 JetBrains s.r.o
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

package consulo.javascript.psi.impl.reference;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.psi.PsiReference;
import consulo.annotations.RequiredReadAction;
import consulo.extensions.CompositeExtensionPointName;

/**
 * @author VISTALL
 * @since 02.12.2015
 */
public interface JSPropertyNameReferenceProvider
{
	CompositeExtensionPointName<JSPropertyNameReferenceProvider> EP_NAME = CompositeExtensionPointName.applicationPoint("consulo.javascript.propertyNameReferenceProvider",
			JSPropertyNameReferenceProvider.class);

	@Nullable
	@RequiredReadAction
	PsiReference getReference(@Nonnull JSProperty property);
}
