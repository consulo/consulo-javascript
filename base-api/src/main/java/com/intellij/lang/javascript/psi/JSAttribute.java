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
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.StubBasedPsiElement;
import consulo.util.collection.ArrayFactory;
import jakarta.annotation.Nonnull;

/**
 * @author  Maxim.Mossienko
 */
public interface JSAttribute extends JSElement, PsiNamedElement, StubBasedPsiElement<JSAttributeStub>
{
	public static final JSAttribute[] EMPTY_ARRAY = new JSAttribute[0];

	public static ArrayFactory<JSAttribute> ARRAY_FACTORY = new ArrayFactory<JSAttribute>()
	{
		@Nonnull
		@Override
		public JSAttribute[] create(int count)
		{
			return count == 0 ? EMPTY_ARRAY : new JSAttribute[count];
		}
	};

	@Override
	String getName();

	JSAttributeNameValuePair[] getValues();

	JSAttributeNameValuePair getValueByName(String name);
}