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

import com.intellij.lang.javascript.psi.JSNamespaceDeclaration;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSNamespaceDeclarationStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSNamespaceDeclarationStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:49:06 PM
 */
public class JSNamespaceDeclarationElementType extends JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration>
{
	private static final JSStubGenerator<JSNamespaceDeclarationStub, JSNamespaceDeclaration> ourStubGenerator = new
			JSStubGenerator<JSNamespaceDeclarationStub, JSNamespaceDeclaration>()
	{
		@Override
		public JSNamespaceDeclarationStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
				final JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> type) throws IOException
		{
			return new JSNamespaceDeclarationStubImpl(dataStream, parentStub, type);
		}

		@Override
		public JSNamespaceDeclarationStub newInstance(final JSNamespaceDeclaration psi, final StubElement parentStub,
				final JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> type)
		{
			return new JSNamespaceDeclarationStubImpl(psi, parentStub, type);
		}
	};

	public JSNamespaceDeclarationElementType()
	{
		super("NAMESPACE_DECLARATION", ourStubGenerator);
	}
}
