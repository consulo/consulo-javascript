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

import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSReferenceListImpl;
import com.intellij.lang.javascript.psi.stubs.JSReferenceListStub;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.ArrayUtil;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 8:00:52 PM
 */
public class JSReferenceListStubImpl extends StubBase<JSReferenceList> implements JSReferenceListStub
{
	private String[] myRefs;

	public JSReferenceListStubImpl(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSReferenceListStub,
			JSReferenceList> type) throws IOException
	{
		super(parentStub, type);

		final int count = dataStream.readInt();

		myRefs = ArrayUtil.newStringArray(count);
		for(int i = 0; i < count; ++i)
		{
			myRefs[i] = dataStream.stringFromId(dataStream.readInt());
		}
	}

	public JSReferenceListStubImpl(final String[] refs, final StubElement parentStub, final JSStubElementType<JSReferenceListStub,
			JSReferenceList> type)
	{
		super(parentStub, type);
		myRefs = refs;
	}

	public JSReferenceListStubImpl(final JSReferenceList psi, final StubElement parentStub, final JSStubElementType<JSReferenceListStub,
			JSReferenceList> type)
	{
		super(parentStub, type);

		final JSReferenceExpression[] referenceExpressions = psi.getExpressions();

		if(referenceExpressions != null)
		{
			myRefs = ArrayUtil.newStringArray(referenceExpressions.length);
			int i = 0;

			for(JSReferenceExpression expr : referenceExpressions)
			{
				myRefs[i++] = expr.getText();
			}
		}
		else
		{
			myRefs = ArrayUtil.EMPTY_STRING_ARRAY;
		}
	}

	@Override
	public JSReferenceList createPsi()
	{
		return new JSReferenceListImpl(this);
	}

	@Override
	public void index(final IndexSink sink)
	{
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		dataStream.writeInt(myRefs.length);
		for(String s : myRefs)
		{
			dataStream.writeInt(dataStream.getStringId(s));
		}
	}

	@Override
	public String[] getReferenceTexts()
	{
		return myRefs;
	}
}