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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.psi.PsiElement;
import com.intellij.psi.SmartPointerManager;
import com.intellij.psi.SmartPsiElementPointer;
import consulo.annotations.Exported;

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

	public JsonPropertyDescriptor(@Nullable String name, @NotNull Object value)
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

	@NotNull
	public JsonPropertyDescriptor deprecated()
	{
		myDeprecated = true;
		return this;
	}

	@NotNull
	public JsonPropertyDescriptor notNull()
	{
		myNullable = false;
		return this;
	}

	@NotNull
	public Object getValue()
	{
		return myValue;
	}

	@NotNull
	public Class getType()
	{
		return myValue instanceof Class ? (Class) myValue : Object.class;
	}

	@Nullable
	public String getName()
	{
		return myName;
	}

	@Exported
	public void setNavigationElement(@NotNull PsiElement element)
	{
		myNavigationElement = SmartPointerManager.getInstance(element.getProject()).createSmartPsiElementPointer(element);
	}

	@Nullable
	public PsiElement getNavigationElement()
	{
		return myNavigationElement == null ? null : myNavigationElement.getElement();
	}
}
