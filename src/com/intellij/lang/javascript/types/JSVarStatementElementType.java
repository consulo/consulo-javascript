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
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.impl.JSVarStatementImpl;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSVarStatementStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 8, 2008
 *         Time: 1:50:59 PM
 */
public class JSVarStatementElementType extends JSStubElementType<JSVarStatementStub, JSVarStatement>
{
	public JSVarStatementElementType()
	{
		super("VAR_STATEMENT");
	}

	@Override
	public boolean shouldCreateStub(ASTNode node)
	{
		final PsiElement element = node.getTreeParent().getPsi();
		return element instanceof JSClass || element instanceof JSPackageStatement || element instanceof JSFile;
	}

	@NotNull
	@Override
	public PsiElement createElement(@NotNull ASTNode astNode)
	{
		return new JSVarStatementImpl(astNode);
	}

	@Override
	public JSVarStatement createPsi(@NotNull JSVarStatementStub stub)
	{
		return new JSVarStatementImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSVarStatementStub createStub(@NotNull JSVarStatement psi, StubElement parentStub)
	{
		return new JSVarStatementStubImpl(parentStub, this);
	}

	@Override
	public void serialize(@NotNull JSVarStatementStub stub, @NotNull StubOutputStream dataStream) throws IOException
	{

	}

	@NotNull
	@Override
	public JSVarStatementStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		return new JSVarStatementStubImpl(parentStub, this);
	}
}
