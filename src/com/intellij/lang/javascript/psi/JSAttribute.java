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

import com.intellij.lang.javascript.psi.stubs.JSAttributeStub;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.StubBasedPsiElement;

/**
 * @by Maxim.Mossienko
 */
public interface JSAttribute extends JSElement, PsiNamedElement, StubBasedPsiElement<JSAttributeStub>
{
	JSAttribute[] EMPTY = new JSAttribute[0];

	@Override
	String getName();

	JSAttributeNameValuePair[] getValues();

	JSAttributeNameValuePair getValueByName(String name);
}