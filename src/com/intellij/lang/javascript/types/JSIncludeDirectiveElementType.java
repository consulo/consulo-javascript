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

import com.intellij.lang.javascript.psi.JSIncludeDirective;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSIncludeDirectiveStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSIncludeDirectiveStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 7, 2008
 *         Time: 9:23:54 PM
 */
public class JSIncludeDirectiveElementType extends JSStubElementType<JSIncludeDirectiveStub, JSIncludeDirective>
{
	private static final JSStubGenerator<JSIncludeDirectiveStub, JSIncludeDirective> ourStubGenerator = new JSStubGenerator<JSIncludeDirectiveStub,
			JSIncludeDirective>()
	{
		@Override
		public JSIncludeDirectiveStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
				final JSStubElementType<JSIncludeDirectiveStub, JSIncludeDirective> type) throws IOException
		{
			return new JSIncludeDirectiveStubImpl(dataStream, parentStub, type);
		}

		@Override
		public JSIncludeDirectiveStub newInstance(final JSIncludeDirective psi, final StubElement parentStub,
				final JSStubElementType<JSIncludeDirectiveStub, JSIncludeDirective> type)
		{
			return new JSIncludeDirectiveStubImpl(psi, parentStub, type);
		}
	};

	public JSIncludeDirectiveElementType()
	{
		super("INCLUDE_DIRECTIVE", ourStubGenerator);
	}
}
