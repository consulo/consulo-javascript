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

import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredDispatchThread;
import org.mustbe.consulo.json.validation.descriptor.JsonPropertyDescriptor;
import org.mustbe.consulo.json.validation.inspections.PropertyValidationInspection;
import com.intellij.codeInsight.navigation.actions.GotoDeclarationHandlerBase;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.openapi.editor.Editor;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiUtilCore;

/**
 * @author VISTALL
 * @since 12.11.2015
 */
public class JsonFileDescriptorGotoDeclarationHandler extends GotoDeclarationHandlerBase
{
	@Nullable
	@Override
	@RequiredDispatchThread
	public PsiElement getGotoDeclarationTarget(PsiElement sourceElement, Editor editor)
	{
		if(PsiUtilCore.getElementType(sourceElement) == JSTokenTypes.STRING_LITERAL)
		{
			PsiElement parent = sourceElement.getParent();
			if(!(parent instanceof JSProperty))
			{
				return null;
			}

			JsonPropertyDescriptor propertyDescriptor = PropertyValidationInspection.findPropertyDescriptor((JSProperty) parent);
			if(propertyDescriptor != null)
			{
				return propertyDescriptor.getNavigationElement();
			}
		}

		return null;
	}
}
