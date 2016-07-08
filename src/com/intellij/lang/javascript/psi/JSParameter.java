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

import consulo.lombok.annotations.ArrayFactoryFields;
import org.jetbrains.annotations.Nullable;
import org.mustbe.consulo.RequiredReadAction;
import com.intellij.psi.PsiElement;

/**
 * User: max
 * Date: Jan 30, 2005
 * Time: 6:43:02 PM
 */
@ArrayFactoryFields
public interface JSParameter extends JSVariable
{
	JSFunction getDeclaringFunction();

	@RequiredReadAction
	boolean isRest();

	@Nullable
	@RequiredReadAction
	PsiElement getRestElement();

	boolean isOptional();
}
