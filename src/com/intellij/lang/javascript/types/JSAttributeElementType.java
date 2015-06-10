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
import com.intellij.lang.javascript.psi.JSAttribute;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSAttributeImpl;
import com.intellij.lang.javascript.psi.stubs.JSAttributeStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSAttributeStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.io.StringRef;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:30:20 PM
 */
public class JSAttributeElementType extends JSStubElementType<JSAttributeStub, JSAttribute>
{
	public JSAttributeElementType()
	{
		super("ATTRIBUTE");
	}

	@NotNull
	@Override
	public PsiElement createElement(@NotNull ASTNode astNode)
	{
		return new JSAttributeImpl(astNode);
	}

	@Override
	public JSAttribute createPsi(@NotNull JSAttributeStub stub)
	{
		return new JSAttributeImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSAttributeStub createStub(@NotNull JSAttribute psi, StubElement parentStub)
	{
		return new JSAttributeStubImpl(psi.getName(), 0, parentStub);
	}

	@Override
	public void serialize(@NotNull JSAttributeStub stub, @NotNull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getName());
	}

	@NotNull
	@Override
	public JSAttributeStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef name = dataStream.readName();
		return new JSAttributeStubImpl(StringRef.toString(name), 0, parentStub);
	}
}
