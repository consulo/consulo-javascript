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
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.impl.JSVariableImpl;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSVariableStubImpl;
import consulo.language.ast.IElementType;
import consulo.language.psi.stub.StubElement;
import consulo.index.io.StringRef;
import consulo.javascript.types.JSQualifiedStubElementType;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;

/**
 * @author Maxim.Mossienko
 * Date: Mar 25, 2008
 * Time: 10:30:10 PM
 */
public class JSVariableElementType extends JSQualifiedStubElementType<JSVariableStub, JSVariable> {
    public JSVariableElementType() {
        super("VARIABLE");
    }

    @Override
    protected boolean doIndexName(JSVariableStub stub, String name, String fqn) {
        IStubElementType stubType = stub.getParentStub().getParentStub().getStubType();

        return stubType instanceof JSPackageStatementElementType || stubType == null;
    }

    @Override
    protected boolean doIndexQualifiedName(JSVariableStub stub, String name, String fqn) {
        return doIndexName(stub, name, fqn);
    }

    @Override
    public boolean shouldCreateStub(ASTNode node) {
        IElementType discriminatingParentType = node.getTreeParent().getTreeParent().getElementType();
        return discriminatingParentType == JSElementTypes.PACKAGE_STATEMENT
            || discriminatingParentType == JSElementTypes.CLASS
            || discriminatingParentType instanceof JSFileElementType;
    }

    @Nonnull
    @Override
    public PsiElement createElement(@Nonnull ASTNode astNode) {
        return new JSVariableImpl(astNode);
    }

    @Override
    public JSVariable createPsi(@Nonnull JSVariableStub stub) {
        return new JSVariableImpl(stub);
    }

    @RequiredReadAction
    @Override
    public JSVariableStub createStub(@Nonnull JSVariable psi, StubElement parentStub) {
        String name = psi.getName();
        int flags = JSVariableStubImpl.buildFlags(psi);
        String typeString = psi.getTypeString();
        String initializerText = psi.getInitializerText();
        String qualifiedName = psi.getQualifiedName();
        return new JSVariableStubImpl(name, flags, typeString, initializerText, qualifiedName, parentStub, this);
    }

    @Override
    public void serialize(@Nonnull JSVariableStub stub, @Nonnull StubOutputStream dataStream) throws IOException {
        dataStream.writeName(stub.getName());
        dataStream.writeVarInt(stub.getFlags());
        dataStream.writeName(stub.getTypeString());
        dataStream.writeName(stub.getInitializerText());
        dataStream.writeName(stub.getQualifiedName());
    }

    @Nonnull
    @Override
    public JSVariableStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException {
        StringRef nameRef = dataStream.readName();
        int flags = dataStream.readVarInt();
        StringRef typeRef = dataStream.readName();
        StringRef initializerRef = dataStream.readName();
        StringRef qualifiedRef = dataStream.readName();
        return new JSVariableStubImpl(
            StringRef.toString(nameRef),
            flags,
            StringRef.toString(typeRef),
            StringRef.toString(initializerRef),
            StringRef.toString(qualifiedRef),
            parentStub,
            this
        );
    }
}
