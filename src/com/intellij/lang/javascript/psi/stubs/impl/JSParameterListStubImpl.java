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

import java.io.DataInputStream;
import java.io.IOException;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.stubs.JSParameterListStub;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 11:29:19 PM
 */
public class JSParameterListStubImpl extends StubBase<JSParameterList> implements JSParameterListStub
{
	public JSParameterListStubImpl(JSParameterList clazz, final StubElement parent, final IStubElementType elementType)
	{
		super(parent, elementType);
	}

	public JSParameterListStubImpl(final DataInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws
			IOException
	{
		super(parentStub, elementType);
	}

	public JSParameterListStubImpl(final StubElement parentStub)
	{
		super(parentStub, JSElementTypes.PARAMETER_LIST);
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
	}
}