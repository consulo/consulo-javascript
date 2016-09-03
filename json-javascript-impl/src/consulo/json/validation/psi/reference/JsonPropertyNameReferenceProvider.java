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

package consulo.json.validation.psi.reference;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import consulo.annotations.RequiredReadAction;
import consulo.json.validation.descriptor.JsonPropertyDescriptor;
import consulo.json.validation.inspections.PropertyValidationInspection;
import com.intellij.lang.javascript.psi.JSProperty;
import consulo.javascript.psi.impl.reference.JSPropertyNameReferenceProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

/**
 * @author VISTALL
 * @since 02.12.2015
 */
public class JsonPropertyNameReferenceProvider implements JSPropertyNameReferenceProvider
{
	@RequiredReadAction
	@Nullable
	@Override
	public PsiReference getReference(@NotNull JSProperty property)
	{
		JsonPropertyDescriptor propertyDescriptor = PropertyValidationInspection.findPropertyDescriptor(property);
		if(propertyDescriptor == null)
		{
			return null;
		}

		PsiElement nameIdentifier = property.getNameIdentifier();
		assert nameIdentifier != null;
		return new JsonPropertyNameReference(property, nameIdentifier, propertyDescriptor);
	}
}
