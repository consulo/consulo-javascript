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
import consulo.annotations.RequiredReadAction;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSImportStatement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSImportStatementImpl;
import com.intellij.lang.javascript.psi.stubs.JSImportStatementStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSImportStatementStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.util.io.StringRef;

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

	@NotNull
	@Override
	public PsiElement createElement(@NotNull ASTNode astNode)
	{
		return new JSImportStatementImpl(astNode);
	}

	@Override
	public JSImportStatement createPsi(@NotNull JSImportStatementStub stub)
	{
		return new JSImportStatementImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSImportStatementStub createStub(@NotNull JSImportStatement psi, StubElement parentStub)
	{
		String importText = psi.getImportText();
		return new JSImportStatementStubImpl(importText, parentStub, this);
	}

	@Override
	public void serialize(@NotNull JSImportStatementStub stub, @NotNull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getImportText());
	}

	@NotNull
	@Override
	public JSImportStatementStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef importText = dataStream.readName();
		return new JSImportStatementStubImpl(StringRef.toString(importText), parentStub, this);
	}
}
