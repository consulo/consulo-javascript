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

import com.intellij.lang.javascript.psi.JSNamespaceDeclaration;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSNamespaceDeclarationStub;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 8:00:52 PM
 */
public class JSNamespaceDeclarationStubImpl extends JSQualifiedObjectStubBase<JSNamespaceDeclaration> implements JSNamespaceDeclarationStub
{
	private String myInitialValueString;

	public JSNamespaceDeclarationStubImpl(final StubInputStream dataStream, final StubElement parentStub,
			final JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> type) throws IOException
	{
		super(dataStream, parentStub, type);
		myInitialValueString = readString(dataStream);
	}

	public JSNamespaceDeclarationStubImpl(final JSNamespaceDeclaration psi, final StubElement parentStub,
			final JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> type)
	{
		super(psi, parentStub, type);
		myInitialValueString = psi.getInitialValueString();
	}

	@Override
	protected int buildFlags(final JSNamespaceDeclaration clazz)
	{
		return 0;
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		super.serialize(dataStream);
		writeString(myInitialValueString, dataStream);
	}

	@Override
	public String getInitialValueString()
	{
		return myInitialValueString;
	}
}
