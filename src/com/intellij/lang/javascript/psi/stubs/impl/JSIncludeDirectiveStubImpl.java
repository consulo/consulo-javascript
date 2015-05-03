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

import com.intellij.lang.javascript.psi.JSIncludeDirective;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSIncludeDirectiveStub;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 8:00:52 PM
 */
public class JSIncludeDirectiveStubImpl extends StubBase<JSIncludeDirective> implements JSIncludeDirectiveStub
{
	private String myIncludeText;

	public JSIncludeDirectiveStubImpl(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSIncludeDirectiveStub,
			JSIncludeDirective> type) throws IOException
	{
		super(parentStub, type);
		final int nameIndex = dataStream.readInt();
		myIncludeText = nameIndex != -1 ? dataStream.stringFromId(nameIndex) : null;
	}

	public JSIncludeDirectiveStubImpl(final JSIncludeDirective psi, final StubElement parentStub, final JSStubElementType<JSIncludeDirectiveStub,
			JSIncludeDirective> type)
	{
		super(parentStub, type);
		myIncludeText = psi.getIncludeText();
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		dataStream.writeInt(myIncludeText != null ? dataStream.getStringId(myIncludeText) : -1);
	}

	@Override
	public String getIncludeText()
	{
		return myIncludeText;
	}
}