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
import com.intellij.lang.javascript.psi.JSAttributeNameValuePair;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSAttributeNameValuePairImpl;
import com.intellij.lang.javascript.psi.stubs.JSAttributeNameValuePairStub;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 8:00:52 PM
 */
public class JSAttributeNameValuePairStubImpl extends JSNamedObjectStubBase<JSAttributeNameValuePair> implements JSAttributeNameValuePairStub
{
	private String myValue;

	public JSAttributeNameValuePairStubImpl(final StubInputStream dataStream, final StubElement parentStub,
			final JSStubElementType<JSAttributeNameValuePairStub, JSAttributeNameValuePair> type) throws IOException
	{
		super(dataStream, parentStub, type);
		final int i = dataStream.readInt();
		if(i != -1)
		{
			myValue = dataStream.stringFromId(i);
		}
	}

	public JSAttributeNameValuePairStubImpl(final JSAttributeNameValuePair psi, final StubElement parentStub,
			final JSStubElementType<JSAttributeNameValuePairStub, JSAttributeNameValuePair> type)
	{
		super(psi, parentStub, type);
		myValue = psi.getSimpleValue();
	}

	public JSAttributeNameValuePairStubImpl(String name, final String value, final StubElement parentStub)
	{
		super(name, 0, parentStub, JSElementTypes.ATTRIBUTE_NAME_VALUE_PAIR);
		myValue = value;
	}

	@Override
	public JSAttributeNameValuePair createPsi()
	{
		return new JSAttributeNameValuePairImpl(this);
	}

	@Override
	public void index(final IndexSink sink)
	{
	}

	@Override
	protected int buildFlags(final JSAttributeNameValuePair clazz)
	{
		return 0;
	}

	@Override
	public String getValue()
	{
		return myValue;
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		super.serialize(dataStream);
		dataStream.writeInt(myValue != null ? dataStream.getStringId(myValue) : -1);
	}
}