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

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.stubs.JSParameterStub;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 11:29:19 PM
 */
public class JSParameterStubImpl extends JSVariableStubBaseImpl<JSParameter> implements JSParameterStub
{
	public static final int REST_MASK = LAST_USED_MASK << 1;
	public static final int OPTIONAL_MASK = LAST_USED_MASK << 2;

	public JSParameterStubImpl(JSParameter clazz, final StubElement parent, final IStubElementType elementType)
	{
		super(clazz, parent, elementType);
	}

	public JSParameterStubImpl(final StubInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws IOException
	{
		super(dataStream, parentStub, elementType);
	}

	public JSParameterStubImpl(final String name, int flags, String type, String initial, final StubElement parentStub)
	{
		super(name, flags | (initial != null ? OPTIONAL_MASK : 0), type, initial, null, parentStub, JSElementTypes.FORMAL_PARAMETER);
	}

	@Override
	protected int buildFlags(final JSParameter clazz)
	{
		final int i = super.buildFlags(clazz);
		return i | (clazz.isRest() ? REST_MASK : 0) | (clazz.isOptional() ? OPTIONAL_MASK : 0);
	}

	@Override
	public boolean isRest()
	{
		return (myFlags & REST_MASK) != 0;
	}

	@Override
	public boolean isOptional()
	{
		return (myFlags & OPTIONAL_MASK) != 0;
	}

	@Override
	protected boolean doIndexName(final String name, final String fqn)
	{
		return false;
	}

	@Override
	protected boolean doIndexQualifiedName(final String name, final String fqn)
	{
		return false;
	}
}