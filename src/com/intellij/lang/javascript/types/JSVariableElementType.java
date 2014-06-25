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

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSVariableStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.tree.IElementType;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:30:10 PM
 */
public class JSVariableElementType extends JSStubElementType<JSVariableStub, JSVariable>
{
	private static final JSStubGenerator<JSVariableStub, JSVariable> ourStubGenerator = new JSStubGenerator<JSVariableStub, JSVariable>()
	{
		@Override
		public JSVariableStub newInstance(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSVariableStub,
				JSVariable> elementType) throws IOException
		{
			return new JSVariableStubImpl(dataStream, parentStub, elementType);
		}

		@Override
		public JSVariableStub newInstance(final JSVariable psi, final StubElement parentStub, final JSStubElementType<JSVariableStub,
				JSVariable> elementType)
		{
			return new JSVariableStubImpl(psi, parentStub, elementType);
		}
	};

	public JSVariableElementType()
	{
		super("VARIABLE", ourStubGenerator);
	}

	@Override
	public boolean shouldCreateStub(final ASTNode node)
	{
		final IElementType discriminatingParentType = node.getTreeParent().getTreeParent().getElementType();
		return discriminatingParentType == JSElementTypes.PACKAGE_STATEMENT ||
				discriminatingParentType == JSElementTypes.CLASS ||
				discriminatingParentType instanceof JSFileElementType;
	}
}
