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

package com.intellij.lang.javascript.psi;

import com.intellij.util.ArrayFactory;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.psi.JavaScriptType;
import consulo.javascript.lang.psi.JavaScriptTypeElement;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author max
 */
public interface JSFunction extends JSQualifiedNamedElement, JSSourceElement, JSAttributeListOwner
{
	public static final JSFunction[] EMPTY_ARRAY = new JSFunction[0];

	public static ArrayFactory<JSFunction> ARRAY_FACTORY = count -> count == 0 ? EMPTY_ARRAY : new JSFunction[count];

	@Nullable
	@RequiredReadAction
	JSParameterList getParameterList();

	JSSourceElement[] getBody();

	@RequiredReadAction
	default boolean isGetProperty()
	{
		return false;
	}

	@RequiredReadAction
	default boolean isSetProperty()
	{
		return false;
	}

	@RequiredReadAction
	default boolean isConstructor()
	{
		return false;
	}

	@Nonnull
	JavaScriptType getReturnType();

	String getReturnTypeString();

	@Nullable
	JavaScriptTypeElement getReturnTypeElement();

	enum FunctionKind
	{
		GETTER,
		SETTER,
		CONSTRUCTOR,
		SIMPLE
	}

	FunctionKind getKind();

	boolean isDeprecated();

	default boolean isReferencesArguments()
	{
		return false;
	}
}
