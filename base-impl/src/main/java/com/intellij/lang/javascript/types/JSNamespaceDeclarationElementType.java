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

import consulo.annotation.access.RequiredReadAction;
import com.intellij.lang.javascript.psi.JSNamespaceDeclaration;
import com.intellij.lang.javascript.psi.impl.JSNamespaceDeclarationImpl;
import com.intellij.lang.javascript.psi.stubs.JSNamespaceDeclarationStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSNamespaceDeclarationStubImpl;
import consulo.language.psi.PsiElement;
import consulo.index.io.StringRef;
import consulo.javascript.types.JSQualifiedStubElementType;
import consulo.language.ast.ASTNode;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;

/**
 * @author Maxim.Mossienko
 * Date: Jun 6, 2008
 * Time: 7:49:06 PM
 */
public class JSNamespaceDeclarationElementType extends JSQualifiedStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> {
    public JSNamespaceDeclarationElementType() {
        super("NAMESPACE_DECLARATION");
    }

    @Override
    public PsiElement createElement(ASTNode astNode) {
        return new JSNamespaceDeclarationImpl(astNode);
    }

    @Override
    public JSNamespaceDeclaration createPsi(JSNamespaceDeclarationStub stub) {
        return new JSNamespaceDeclarationImpl(stub);
    }

    @RequiredReadAction
    @Override
    public JSNamespaceDeclarationStub createStub(JSNamespaceDeclaration psi, StubElement parentStub) {
        String name = psi.getName();
        String qualifiedName = psi.getQualifiedName();
        String initialValueString = psi.getInitialValueString();
        return new JSNamespaceDeclarationStubImpl(name, qualifiedName, initialValueString, parentStub, this);
    }

    @Override
    public void serialize(JSNamespaceDeclarationStub stub, StubOutputStream dataStream) throws IOException {
        dataStream.writeName(stub.getName());
        dataStream.writeName(stub.getQualifiedName());
        dataStream.writeName(stub.getInitialValueString());
    }

    @Override
    public JSNamespaceDeclarationStub deserialize(StubInputStream dataStream, StubElement parentStub) throws IOException {
        StringRef nameRef = dataStream.readName();
        StringRef qualifiedRef = dataStream.readName();
        StringRef initialValueRef = dataStream.readName();
        return new JSNamespaceDeclarationStubImpl(
            StringRef.toString(nameRef),
            StringRef.toString(qualifiedRef),
            StringRef.toString(initialValueRef),
            parentStub,
            this
        );
    }
}
