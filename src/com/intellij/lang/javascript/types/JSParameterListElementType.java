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

import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSParameterListStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSParameterListStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:30:00 PM
 */
public class JSParameterListElementType extends JSStubElementType<JSParameterListStub, JSParameterList>
{
	private static final JSStubGenerator<JSParameterListStub, JSParameterList> ourStubGenerator = new JSStubGenerator<JSParameterListStub,
			JSParameterList>()
	{
		@Override
		public JSParameterListStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
				final JSStubElementType<JSParameterListStub, JSParameterList> elementType) throws IOException
		{
			return new JSParameterListStubImpl(dataStream, parentStub, elementType);
		}

		@Override
		public JSParameterListStub newInstance(final JSParameterList psi, final StubElement parentStub, final JSStubElementType<JSParameterListStub,
				JSParameterList> elementType)
		{
			return new JSParameterListStubImpl(psi, parentStub, elementType);
		}
	};

	public JSParameterListElementType()
	{
		super("PARAMETER_LIST", ourStubGenerator);
	}
}
