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

import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.JSUseNamespaceDirective;
import com.intellij.lang.javascript.psi.stubs.JSUseNamespaceDirectiveStub;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 8:00:52 PM
 */
public class JSUseNamespaceDirectiveStubImpl extends StubBase<JSUseNamespaceDirective> implements JSUseNamespaceDirectiveStub
{
	private String myNamespaceToUse;

	public JSUseNamespaceDirectiveStubImpl(final StubInputStream dataStream, final StubElement parentStub,
			final JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> type) throws IOException
	{
		super(parentStub, type);
		final int idx = dataStream.readInt();
		myNamespaceToUse = idx != -1 ? dataStream.stringFromId(idx) : null;
	}

	public JSUseNamespaceDirectiveStubImpl(final JSUseNamespaceDirective psi, final StubElement parentStub,
			final JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> type)
	{
		super(parentStub, type);

		myNamespaceToUse = psi.getNamespaceToBeUsed();
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		dataStream.writeInt(myNamespaceToUse != null ? dataStream.getStringId(myNamespaceToUse) : -1);
	}

	@Override
	public String getNamespaceToUse()
	{
		return myNamespaceToUse;
	}
}
