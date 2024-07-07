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

import jakarta.annotation.Nonnull;

import consulo.annotation.access.RequiredReadAction;
import com.intellij.lang.javascript.psi.JSImportStatement;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSImportStatementImpl;
import com.intellij.lang.javascript.psi.stubs.JSImportStatementStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSImportStatementStubImpl;
import consulo.index.io.StringRef;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:49:18 PM
 */
public class JSImportStatementElementType extends JSStubElementType<JSImportStatementStub, JSImportStatement>
{
	public JSImportStatementElementType()
	{
		super("IMPORT_STATEMENT");
	}

	@Nonnull
	@Override
	public PsiElement createElement(@Nonnull ASTNode astNode)
	{
		return new JSImportStatementImpl(astNode);
	}

	@Override
	public JSImportStatement createPsi(@Nonnull JSImportStatementStub stub)
	{
		return new JSImportStatementImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSImportStatementStub createStub(@Nonnull JSImportStatement psi, StubElement parentStub)
	{
		String importText = psi.getImportText();
		return new JSImportStatementStubImpl(importText, parentStub, this);
	}

	@Override
	public void serialize(@Nonnull JSImportStatementStub stub, @Nonnull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getImportText());
	}

	@Nonnull
	@Override
	public JSImportStatementStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef importText = dataStream.readName();
		return new JSImportStatementStubImpl(StringRef.toString(importText), parentStub, this);
	}
}
