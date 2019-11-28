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
import consulo.annotation.access.RequiredReadAction;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSParameterImpl;
import com.intellij.lang.javascript.psi.stubs.JSParameterStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSParameterStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.io.StringRef;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:30:07 PM
 */
public class JSParameterElementType extends JSStubElementType<JSParameterStub, JSParameter>
{
	public JSParameterElementType()
	{
		super("FORMAL_PARAMETER");
	}

	@Nonnull
	@Override
	public PsiElement createElement(@Nonnull ASTNode astNode)
	{
		return new JSParameterImpl(astNode);
	}

	@Override
	public JSParameter createPsi(@Nonnull JSParameterStub stub)
	{
		return new JSParameterImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSParameterStub createStub(@Nonnull JSParameter psi, StubElement parentStub)
	{
		String name = psi.getName();
		int flags = JSParameterStubImpl.buildFlags(psi);
		String typeString = psi.getTypeString();
		String initializerText = psi.getInitializerText();
		String qualifiedName = psi.getQualifiedName();
		return new JSParameterStubImpl(name, flags, typeString, initializerText, qualifiedName, parentStub, this);
	}

	@Override
	public void serialize(@Nonnull JSParameterStub stub, @Nonnull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getName());
		dataStream.writeVarInt(stub.getFlags());
		dataStream.writeName(stub.getTypeString());
		dataStream.writeName(stub.getInitializerText());
		dataStream.writeName(stub.getQualifiedName());
	}

	@Nonnull
	@Override
	public JSParameterStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef nameRef = dataStream.readName();
		int flags = dataStream.readVarInt();
		StringRef typeRef = dataStream.readName();
		StringRef initializerRef = dataStream.readName();
		StringRef qualifiedRef = dataStream.readName();
		return new JSParameterStubImpl(StringRef.toString(nameRef), flags, StringRef.toString(typeRef), StringRef.toString(initializerRef),
				StringRef.toString(qualifiedRef), parentStub, this);
	}
}
