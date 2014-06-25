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

import org.jetbrains.annotations.NonNls;
import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSReferenceListStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSReferenceListStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:51:24 PM
 */
public class JSReferenceListElementType extends JSStubElementType<JSReferenceListStub, JSReferenceList>
{
	private static final JSStubGenerator<JSReferenceListStub, JSReferenceList> ourStubGenerator = new JSStubGenerator<JSReferenceListStub,
			JSReferenceList>()
	{
		@Override
		public JSReferenceListStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
				final JSStubElementType<JSReferenceListStub, JSReferenceList> type) throws IOException
		{
			return new JSReferenceListStubImpl(dataStream, parentStub, type);
		}

		@Override
		public JSReferenceListStub newInstance(final JSReferenceList psi, final StubElement parentStub, final JSStubElementType<JSReferenceListStub,
				JSReferenceList> type)
		{
			return new JSReferenceListStubImpl(psi, parentStub, type);
		}
	};

	public JSReferenceListElementType(@NonNls String name)
	{
		super(name, ourStubGenerator);
	}
}
