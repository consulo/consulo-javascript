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

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import consulo.annotations.RequiredReadAction;
import org.mustbe.consulo.javascript.lang.psi.JavaScriptType;
import org.mustbe.consulo.javascript.lang.psi.JavaScriptTypeElement;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.psi.StubBasedPsiElement;
import consulo.lombok.annotations.ArrayFactoryFields;

/**
 * @author max
 */
@ArrayFactoryFields
public interface JSFunction extends JSQualifiedNamedElement, JSSourceElement, JSAttributeListOwner, StubBasedPsiElement<JSFunctionStub>
{
	@Nullable
	@RequiredReadAction
	JSParameterList getParameterList();

	JSSourceElement[] getBody();

	@RequiredReadAction
	boolean isGetProperty();

	@RequiredReadAction
	boolean isSetProperty();

	@RequiredReadAction
	boolean isConstructor();

	@NotNull
	JavaScriptType getReturnType();

	String getReturnTypeString();

	@Nullable
	JavaScriptTypeElement getReturnTypeElement();

	enum FunctionKind
	{
		GETTER, SETTER, CONSTRUCTOR, SIMPLE
	}

	FunctionKind getKind();

	boolean isDeprecated();

	boolean isReferencesArguments();
}
