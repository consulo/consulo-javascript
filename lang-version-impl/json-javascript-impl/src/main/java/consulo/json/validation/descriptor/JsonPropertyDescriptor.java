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

package consulo.json.validation.descriptor;

import com.intellij.psi.PsiElement;
import com.intellij.psi.SmartPointerManager;
import com.intellij.psi.SmartPsiElementPointer;
import consulo.annotation.UsedInPlugin;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 10.11.2015
 */
public class JsonPropertyDescriptor
{
	private SmartPsiElementPointer<?> myNavigationElement;

	private String myName;
	private Object myValue;
	private boolean myDeprecated;
	private boolean myNullable = true;

	public JsonPropertyDescriptor(@Nullable String name, @Nonnull Object value)
	{
		myName = name;
		myValue = value;
	}

	public boolean isNullable()
	{
		return myNullable;
	}

	public boolean isDeprecated()
	{
		return myDeprecated;
	}

	@Nonnull
	public JsonPropertyDescriptor deprecated()
	{
		myDeprecated = true;
		return this;
	}

	@Nonnull
	public JsonPropertyDescriptor notNull()
	{
		myNullable = false;
		return this;
	}

	@Nonnull
	public Object getValue()
	{
		return myValue;
	}

	@Nullable
	public String getName()
	{
		return myName;
	}

	@UsedInPlugin
	public void setNavigationElement(@Nonnull PsiElement element)
	{
		myNavigationElement = SmartPointerManager.getInstance(element.getProject()).createSmartPsiElementPointer(element);
	}

	@Nullable
	public PsiElement getNavigationElement()
	{
		return myNavigationElement == null ? null : myNavigationElement.getElement();
	}
}
