/*
 * Copyright 2000-2005 JetBrains s.r.o
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

package com.intellij.lang.javascript.psi.stubs.impl;

import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 7:11:48 PM
 */
public abstract class JSNamedObjectStubBase<T extends PsiNamedElement> extends StubBase<T>
{
	protected final String myName;
	protected final int myFlags;

	protected JSNamedObjectStubBase(String name, int flags, final StubElement parent, final IStubElementType elementType)
	{
		super(parent, elementType);

		myName = name;
		myFlags = flags;
	}

	public int getFlags()
	{
		return myFlags;
	}

	public String getName()
	{
		return myName;
	}
}