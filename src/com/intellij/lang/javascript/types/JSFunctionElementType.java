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

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSFunctionImpl;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSFunctionStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:50:34 PM
 */
public class JSFunctionElementType extends JSQualifiedStubElementType<JSFunctionStub, JSFunction>
{
	public JSFunctionElementType()
	{
		super("FUNCTION_DECLARATION");
	}

	@Override
	protected boolean doIndexName(JSFunctionStub stub, final String name, final String fqn)
	{
		final IStubElementType stubType = stub.getParentStub().getStubType();

		if(stubType instanceof JSPackageStatementElementType || stubType == null)
		{
			return true;
		}
		return false;
	}

	@Override
	protected boolean doIndexQualifiedName(JSFunctionStub stub, String name, String fqn)
	{
		return doIndexName(stub, name, fqn);
	}

	@Override
	public JSFunctionStub newInstance(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSFunctionStub,
			JSFunction> elementType) throws IOException
	{
		return new JSFunctionStubImpl(dataStream, parentStub, elementType);
	}

	@Override
	public JSFunctionStub newInstance(final JSFunction psi, final StubElement parentStub, final JSStubElementType<JSFunctionStub,
			JSFunction> elementType)
	{
		return new JSFunctionStubImpl(psi, parentStub, elementType);
	}

	@NotNull
	@Override
	public PsiElement createElement(@NotNull ASTNode astNode)
	{
		return new JSFunctionImpl(astNode);
	}

	@Override
	public JSFunction createPsi(@NotNull JSFunctionStub stub)
	{
		return new JSFunctionImpl(stub, this);
	}
}
