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
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.impl.JSVariableImpl;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSVariableStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.io.StringRef;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:30:10 PM
 */
public class JSVariableElementType extends JSQualifiedStubElementType<JSVariableStub, JSVariable>
{
	public JSVariableElementType()
	{
		super("VARIABLE");
	}

	@Override
	protected boolean doIndexName(JSVariableStub stub, String name, String fqn)
	{
		final IStubElementType stubType = stub.getParentStub().getParentStub().getStubType();

		if(stubType instanceof JSPackageStatementElementType || stubType == null)
		{
			return true;
		}
		return false;
	}

	@Override
	protected boolean doIndexQualifiedName(JSVariableStub stub, String name, String fqn)
	{
		return doIndexName(stub, name, fqn);
	}

	@Override
	public boolean shouldCreateStub(final ASTNode node)
	{
		final IElementType discriminatingParentType = node.getTreeParent().getTreeParent().getElementType();
		return discriminatingParentType == JSElementTypes.PACKAGE_STATEMENT ||
				discriminatingParentType == JSElementTypes.CLASS ||
				discriminatingParentType instanceof JSFileElementType;
	}

	@NotNull
	@Override
	public PsiElement createElement(@NotNull ASTNode astNode)
	{
		return new JSVariableImpl(astNode);
	}

	@Override
	public JSVariable createPsi(@NotNull JSVariableStub stub)
	{
		return new JSVariableImpl(stub);
	}

	@RequiredReadAction
	@Override
	public JSVariableStub createStub(@NotNull JSVariable psi, StubElement parentStub)
	{
		String name = psi.getName();
		int flags = JSVariableStubImpl.buildFlags(psi);
		String typeString = psi.getTypeString();
		String initializerText = psi.getInitializerText();
		String qualifiedName = psi.getQualifiedName();
		return new JSVariableStubImpl(name, flags, typeString, initializerText, qualifiedName, parentStub, this);
	}

	@Override
	public void serialize(@NotNull JSVariableStub stub, @NotNull StubOutputStream dataStream) throws IOException
	{
		dataStream.writeName(stub.getName());
		dataStream.writeVarInt(stub.getFlags());
		dataStream.writeName(stub.getTypeString());
		dataStream.writeName(stub.getInitializerText());
		dataStream.writeName(stub.getQualifiedName());
	}

	@NotNull
	@Override
	public JSVariableStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException
	{
		StringRef nameRef = dataStream.readName();
		int flags = dataStream.readVarInt();
		StringRef typeRef = dataStream.readName();
		StringRef initializerRef = dataStream.readName();
		StringRef qualifiedRef = dataStream.readName();
		return new JSVariableStubImpl(StringRef.toString(nameRef), flags, StringRef.toString(typeRef), StringRef.toString(initializerRef),
				StringRef.toString(qualifiedRef), parentStub, this);
	}
}
