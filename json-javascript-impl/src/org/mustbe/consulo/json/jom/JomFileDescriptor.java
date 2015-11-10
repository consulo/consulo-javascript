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

package org.mustbe.consulo.json.jom;

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import com.intellij.openapi.extensions.ExtensionPointName;
import com.intellij.psi.PsiFile;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public abstract class JomFileDescriptor<T extends JomElement>
{
	public static ExtensionPointName<JomFileDescriptor> EP_NAME = new ExtensionPointName<JomFileDescriptor>("org.mustbe.consulo.javascript.jomFileDescriptor");

	private Class<T> myDefinitionClass;

	public JomFileDescriptor(@NotNull Class<T> definitionClass)
	{
		myDefinitionClass = definitionClass;
	}

	@NotNull
	public Class<T> getDefinitionClass()
	{
		return myDefinitionClass;
	}

	@NotNull
	public abstract Icon getIcon();

	public abstract boolean isMyFile(@NotNull PsiFile psiFile);
}
