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

package com.intellij.lang.javascript.types;

import java.io.IOException;

import javax.annotation.Nonnull;

import consulo.annotations.RequiredReadAction;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSFunctionStubImpl;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.io.StringRef;
import consulo.javascript.types.JSQualifiedStubElementType;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:50:34 PM
 */
public abstract class JSFunctionElementType extends JSQualifiedStubElementType<JSFunctionStub, JSFunction>
{
	public JSFunctionElementType(@Nonnull String name)
	{
		super(name);
	}

	@Override
	protected boolean doIndexName(JSFunctionStub stub, final String name, final String fqn)
	{
		final IStubElementType stubType = stub.getParentStub().getStubType();

		if(stubType instanceof JSPackageStatementElementType || stubType == null)
		{
			return true;
		}
		return false;
	}

	@Override
	protected boolean doIndexQualifiedName(JSFunctionStub stub, String name, String fqn)
	{
		return doIndexName(stub, name, fqn);
	}

	@RequiredReadAction
	@Override
	public JSFunctionStub createStub(@Nonnull JSFunction psi, StubElement parentStub)
	{
		String name = psi.getName();
		String qualifiedName = psi.getQualifiedName();
		String returnTypeString = psi.getReturnTypeString();
		int flags = JSFunctionStubImpl.buildFlags(psi);
		return new JSFunctionStubImpl(name, flags, qualifiedName, returnTypeString, parentStub, this);
	}

	@Override
	public void serialize(@Nonnull JSFunctionStub stub, @Nonnull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getName());
		dataStream.writeName(stub.getQualifiedName());
		dataStream.writeName(stub.getReturnTypeString());
		dataStream.writeVarInt(stub.getFlags());
	}

	@Nonnull
	@Override
	public JSFunctionStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef nameRef = dataStream.readName();
		StringRef qualifiedRef = dataStream.readName();
		StringRef returnTypeRef = dataStream.readName();
		int flags = dataStream.readVarInt();
		return new JSFunctionStubImpl(StringRef.toString(nameRef), flags, StringRef.toString(qualifiedRef), StringRef.toString(returnTypeRef),
				parentStub, this);
	}
}
