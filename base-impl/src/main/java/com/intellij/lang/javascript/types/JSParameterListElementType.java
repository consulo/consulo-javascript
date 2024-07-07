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

import com.intellij.lang.javascript.psi.JSParameterList;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSParameterListImpl;
import com.intellij.lang.javascript.psi.stubs.JSParameterListStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSParameterListStubImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;

import jakarta.annotation.Nonnull;
import java.io.IOException;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:30:00 PM
 */
public class JSParameterListElementType extends JSStubElementType<JSParameterListStub, JSParameterList>
{
	public JSParameterListElementType()
	{
		super("PARAMETER_LIST");
	}

	@Nonnull
	@Override
	public PsiElement createElement(@Nonnull ASTNode astNode)
	{
		return new JSParameterListImpl(astNode);
	}

	@Override
	public JSParameterList createPsi(@Nonnull JSParameterListStub stub)
	{
		return new JSParameterListImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSParameterListStub createStub(@Nonnull JSParameterList psi, StubElement parentStub)
	{
		return new JSParameterListStubImpl(parentStub, this);
	}

	@Override
	public void serialize(@Nonnull JSParameterListStub stub, @Nonnull StubOutputStream dataStream) throws IOException
	{

	}

	@Nonnull
	@Override
	public JSParameterListStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		return new JSParameterListStubImpl(parentStub, this);
	}
}
