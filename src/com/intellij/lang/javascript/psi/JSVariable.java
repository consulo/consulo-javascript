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

import com.intellij.lang.javascript.psi.stubs.JSVariableStubBase;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import com.intellij.util.IncorrectOperationException;

/**
 * @author max
 *         Date: Jan 30, 2005
 *         Time: 6:43:42 PM
 */
public interface JSVariable extends JSQualifiedNamedElement, JSAttributeListOwner, StubBasedPsiElement<JSVariableStubBase>
{
	JSVariable[] EMPTY_ARRAY = new JSVariable[0];

	boolean hasInitializer();

	JSExpression getInitializer();

	String getInitializerText();

	void setInitializer(JSExpression expr) throws IncorrectOperationException;

	JSType getType();

	String getTypeString();

	PsiElement getTypeElement();

	boolean isConst();

	boolean isLocal();

	boolean isDeprecated();
}
