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

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSAttributeNameValuePair;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSAttributeNameValuePairImpl;
import com.intellij.lang.javascript.psi.stubs.JSAttributeNameValuePairStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSAttributeNameValuePairStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.io.StringRef;

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

	@NotNull
	@Override
	public PsiElement createElement(@NotNull ASTNode astNode)
	{
		return new JSAttributeNameValuePairImpl(astNode);
	}

	@Override
	public JSAttributeNameValuePair createPsi(@NotNull JSAttributeNameValuePairStub stub)
	{
		return new JSAttributeNameValuePairImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSAttributeNameValuePairStub createStub(@NotNull JSAttributeNameValuePair psi, StubElement parentStub)
	{
		String name = psi.getName();
		String simpleValue = psi.getSimpleValue();
		return new JSAttributeNameValuePairStubImpl(name, simpleValue, parentStub);
	}

	@Override
	public void serialize(@NotNull JSAttributeNameValuePairStub stub, @NotNull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getName());
		dataStream.writeName(stub.getValue());
	}

	@NotNull
	@Override
	public JSAttributeNameValuePairStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef name = dataStream.readName();
		StringRef value = dataStream.readName();
		return new JSAttributeNameValuePairStubImpl(StringRef.toString(name), StringRef.toString(value), parentStub);
	}
}
