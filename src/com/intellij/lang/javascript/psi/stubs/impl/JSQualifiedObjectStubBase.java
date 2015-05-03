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

package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedStub;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 7:11:48 PM
 */
abstract class JSQualifiedObjectStubBase<T extends JSQualifiedNamedElement> extends JSNamedObjectStubBase<T> implements JSQualifiedStub<T>
{
	protected final String myQualifiedName;

	protected JSQualifiedObjectStubBase(T clazz, final StubElement parent, final IStubElementType elementType)
	{
		super(clazz, parent, elementType);

		myQualifiedName = doGetQualifiedName(clazz);
	}

	protected JSQualifiedObjectStubBase(String name, int flags, String qName, final StubElement parent, final IStubElementType elementType)
	{
		super(name, flags, parent, elementType);

		myQualifiedName = qName;
	}

	protected String doGetQualifiedName(final T clazz)
	{
		return clazz.getQualifiedName();
	}

	protected JSQualifiedObjectStubBase(final StubInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws
			IOException
	{
		super(dataStream, parentStub, elementType);

		myQualifiedName = readString(dataStream);
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		super.serialize(dataStream);
		writeString(myQualifiedName, dataStream);
	}

	@Override
	public String getQualifiedName()
	{
		return myQualifiedName;
	}
}