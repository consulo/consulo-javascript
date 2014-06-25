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

package com.intellij.lang.javascript.types;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSAttributeListStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSAttributeListStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:30:17 PM
 */
public class JSAttributeListElementType extends JSStubElementType<JSAttributeListStub, JSAttributeList>
{
	private static final JSStubGenerator<JSAttributeListStub, JSAttributeList> ourStubGenerator = new JSStubGenerator<JSAttributeListStub,
			JSAttributeList>()
	{
		@Override
		public JSAttributeListStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
				final JSStubElementType<JSAttributeListStub, JSAttributeList> elementType) throws IOException
		{
			return new JSAttributeListStubImpl(dataStream, parentStub, elementType);
		}

		@Override
		public JSAttributeListStub newInstance(final JSAttributeList psi, final StubElement parentStub, final JSStubElementType<JSAttributeListStub,
				JSAttributeList> elementType)
		{
			return new JSAttributeListStubImpl(psi, parentStub, elementType);
		}
	};

	public JSAttributeListElementType()
	{
		super("ATTRIBUTE_LIST", ourStubGenerator);
	}
}
