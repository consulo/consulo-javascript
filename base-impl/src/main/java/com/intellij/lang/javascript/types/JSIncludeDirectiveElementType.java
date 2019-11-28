/*
 * Copyright 2000-2005 JetBrains s.r.o
 * Copyright 2013-2015 must-be.org
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

import javax.annotation.Nonnull;

import consulo.annotation.access.RequiredReadAction;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSIncludeDirective;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSIncludeDirectiveImpl;
import com.intellij.lang.javascript.psi.stubs.JSIncludeDirectiveStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSIncludeDirectiveStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.io.StringRef;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 7, 2008
 *         Time: 9:23:54 PM
 */
public class JSIncludeDirectiveElementType extends JSStubElementType<JSIncludeDirectiveStub, JSIncludeDirective>
{
	public JSIncludeDirectiveElementType()
	{
		super("INCLUDE_DIRECTIVE");
	}

	@Override
	public void serialize(@Nonnull JSIncludeDirectiveStub stub, @Nonnull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getIncludeText());
	}

	@Nonnull
	@Override
	public JSIncludeDirectiveStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef includeText = dataStream.readName();
		return new JSIncludeDirectiveStubImpl(StringRef.toString(includeText), parentStub, this);
	}

	@RequiredReadAction
	@Override
	public JSIncludeDirectiveStub createStub(@Nonnull JSIncludeDirective psi, StubElement parentStub)
	{
		String includeText = psi.getIncludeText();
		return new JSIncludeDirectiveStubImpl(includeText, parentStub, this);
	}

	@Nonnull
	@Override
	public PsiElement createElement(@Nonnull ASTNode astNode)
	{
		return new JSIncludeDirectiveImpl(astNode);
	}

	@Override
	public JSIncludeDirective createPsi(@Nonnull JSIncludeDirectiveStub stub)
	{
		return new JSIncludeDirectiveImpl(stub);
	}
}
