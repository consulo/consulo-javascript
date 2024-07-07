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

import jakarta.annotation.Nonnull;

import consulo.annotation.access.RequiredReadAction;
import com.intellij.lang.javascript.psi.JSAttributeNameValuePair;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSAttributeNameValuePairImpl;
import com.intellij.lang.javascript.psi.stubs.JSAttributeNameValuePairStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSAttributeNameValuePairStubImpl;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.StubElement;
import consulo.index.io.StringRef;
import consulo.language.ast.ASTNode;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 8, 2008
 *         Time: 6:20:05 PM
 */
public class JSAttributeNameValuePairType extends JSStubElementType<JSAttributeNameValuePairStub, JSAttributeNameValuePair>
{
	public JSAttributeNameValuePairType()
	{
		super("ATTRIBUTE_NAME_VALUE_PAIR");
	}

	@Nonnull
	@Override
	public PsiElement createElement(@Nonnull ASTNode astNode)
	{
		return new JSAttributeNameValuePairImpl(astNode);
	}

	@Override
	public JSAttributeNameValuePair createPsi(@Nonnull JSAttributeNameValuePairStub stub)
	{
		return new JSAttributeNameValuePairImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSAttributeNameValuePairStub createStub(@Nonnull JSAttributeNameValuePair psi, StubElement parentStub)
	{
		String name = psi.getName();
		String simpleValue = psi.getSimpleValue();
		return new JSAttributeNameValuePairStubImpl(name, simpleValue, parentStub);
	}

	@Override
	public void serialize(@Nonnull JSAttributeNameValuePairStub stub, @Nonnull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getName());
		dataStream.writeName(stub.getValue());
	}

	@Nonnull
	@Override
	public JSAttributeNameValuePairStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef name = dataStream.readName();
		StringRef value = dataStream.readName();
		return new JSAttributeNameValuePairStubImpl(StringRef.toString(name), StringRef.toString(value), parentStub);
	}
}
