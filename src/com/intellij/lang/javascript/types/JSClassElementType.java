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
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSClassImpl;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSClassStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:30:14 PM
 */
public class JSClassElementType extends JSStubElementType<JSClassStub, JSClass>
{
	public JSClassElementType()
	{
		super("CLASS");
	}

	@Override
	public JSClassStub newInstance(final StubInputStream dataStream,
			final StubElement parentStub,
			final JSStubElementType<JSClassStub, JSClass> elementType) throws IOException
	{
		return new JSClassStubImpl(dataStream, parentStub, elementType);
	}

	@Override
	public JSClassStub newInstance(final JSClass psi, final StubElement parentStub, final JSStubElementType<JSClassStub, JSClass> elementType)
	{
		return new JSClassStubImpl(psi, parentStub, elementType);
	}

	@NotNull
	@Override
	public PsiElement createElement(@NotNull ASTNode astNode)
	{
		return new JSClassImpl(astNode);
	}

	@Override
	public JSClass createPsi(@NotNull JSClassStub stub)
	{
		return new JSClassImpl(stub);
	}
}
