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

import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSPackageStatementImpl;
import com.intellij.lang.javascript.psi.stubs.JSPackageStatementStub;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 8:00:52 PM
 */
public class JSPackageStatementStubImpl extends JSQualifiedObjectStubBase<JSPackageStatement> implements JSPackageStatementStub
{
	public JSPackageStatementStubImpl(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSPackageStatementStub,
			JSPackageStatement> type) throws IOException
	{
		super(dataStream, parentStub, type);
	}

	public JSPackageStatementStubImpl(final JSPackageStatement psi, final StubElement parentStub, final JSStubElementType<JSPackageStatementStub,
			JSPackageStatement> type)
	{
		super(psi, parentStub, type);
	}

	@Override
	public JSPackageStatement createPsi()
	{
		return new JSPackageStatementImpl(this);
	}

	@Override
	protected int buildFlags(final JSPackageStatement clazz)
	{
		return 0;
	}

	@Override
	protected boolean doIndexName(String name, String fqn)
	{
		return false;
	}

	@Override
	protected boolean doIndexQualifiedName(String name, String fqn)
	{
		return false;
	}
}