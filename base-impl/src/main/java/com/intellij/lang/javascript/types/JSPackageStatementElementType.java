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
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSPackageStatementImpl;
import com.intellij.lang.javascript.psi.stubs.JSPackageStatementStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSPackageStatementStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.io.StringRef;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:49:25 PM
 */
public class JSPackageStatementElementType extends JSStubElementType<JSPackageStatementStub, JSPackageStatement>
{
	public JSPackageStatementElementType()
	{
		super("PACKAGE_STATEMENT");
	}

	@Nonnull
	@Override
	public PsiElement createElement(@Nonnull ASTNode astNode)
	{
		return new JSPackageStatementImpl(astNode);
	}

	@Override
	public JSPackageStatement createPsi(@Nonnull JSPackageStatementStub stub)
	{
		return new JSPackageStatementImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSPackageStatementStub createStub(@Nonnull JSPackageStatement psi, StubElement parentStub)
	{
		String name = psi.getName();
		String qualifiedName = psi.getQualifiedName();
		return new JSPackageStatementStubImpl(name, qualifiedName, parentStub, this);
	}

	@Override
	public void serialize(@Nonnull JSPackageStatementStub stub, @Nonnull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getName());
		dataStream.writeName(stub.getQualifiedName());
	}

	@Nonnull
	@Override
	public JSPackageStatementStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef nameRef = dataStream.readName();
		StringRef qualifiedRef = dataStream.readName();
		return new JSPackageStatementStubImpl(StringRef.toString(nameRef), StringRef.toString(qualifiedRef), parentStub, this);
	}
}
