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

import org.jetbrains.annotations.NotNull;
import org.mustbe.consulo.RequiredReadAction;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.JSUseNamespaceDirective;
import com.intellij.lang.javascript.psi.impl.JSUseNamespaceDirectiveImpl;
import com.intellij.lang.javascript.psi.stubs.JSUseNamespaceDirectiveStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSUseNamespaceDirectiveStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.io.StringRef;

/**
 * @author Maxim.Mossienko
 *         Date: Oct 3, 2008
 *         Time: 9:13:01 PM
 */
public class JSUseNamespaceDirectiveType extends JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective>
{
	public JSUseNamespaceDirectiveType()
	{
		super("USE_NAMESPACE_DIRECTIVE");
	}

	@NotNull
	@Override
	public PsiElement createElement(@NotNull ASTNode astNode)
	{
		return new JSUseNamespaceDirectiveImpl(astNode);
	}

	@Override
	public JSUseNamespaceDirective createPsi(@NotNull JSUseNamespaceDirectiveStub stub)
	{
		return new JSUseNamespaceDirectiveImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSUseNamespaceDirectiveStub createStub(@NotNull JSUseNamespaceDirective psi, StubElement parentStub)
	{
		return new JSUseNamespaceDirectiveStubImpl(psi.getNamespaceToBeUsed(), parentStub, this);
	}

	@Override
	public void serialize(@NotNull JSUseNamespaceDirectiveStub stub, @NotNull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getNamespaceToUse());
	}

	@NotNull
	@Override
	public JSUseNamespaceDirectiveStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef nameRef = dataStream.readName();
		return new JSUseNamespaceDirectiveStubImpl(StringRef.toString(nameRef), parentStub, this);
	}
}
