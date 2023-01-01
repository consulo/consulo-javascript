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

import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.language.util.IncorrectOperationException;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author max
 *         Date: Jan 30, 2005
 *         Time: 6:43:42 PM
 */
public interface JSVariable extends JSQualifiedNamedElement, JSAttributeListOwner
{
	JSVariable[] EMPTY_ARRAY = new JSVariable[0];

	boolean hasInitializer();

	@RequiredReadAction
	@Nullable
	JSExpression getInitializer();

	String getInitializerText();

	void setInitializer(JSExpression expr) throws IncorrectOperationException;

	@Nonnull
	JavaScriptType getType();

	@Nullable
	@Deprecated
	String getTypeString();

	@Nullable
	@RequiredReadAction
	JavaScriptTypeElement getTypeElement();

	boolean isConst();

	boolean isLocal();

	boolean isDeprecated();
}
