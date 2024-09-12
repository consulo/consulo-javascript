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

import com.intellij.lang.javascript.psi.JSAttributeList;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSAttributeListImpl;
import com.intellij.lang.javascript.psi.stubs.JSAttributeListStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSAttributeListStubImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.index.io.StringRef;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;

import jakarta.annotation.Nonnull;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
 * Date: Mar 25, 2008
 * Time: 10:30:17 PM
 */
public class JSAttributeListElementType extends JSStubElementType<JSAttributeListStub, JSAttributeList> {
    public JSAttributeListElementType() {
        super("ATTRIBUTE_LIST");
    }

    @Nonnull
    @Override
    public PsiElement createElement(@Nonnull ASTNode astNode) {
        return new JSAttributeListImpl(astNode);
    }

    @Override
    public JSAttributeList createPsi(@Nonnull JSAttributeListStub stub) {
        return new JSAttributeListImpl(stub);
    }

    @RequiredReadAction
    @Override
    public JSAttributeListStub createStub(@Nonnull JSAttributeList psi, StubElement parentStub) {
        String namespace = psi.getNamespace();
        int flags = JSAttributeListStubImpl.getFlags(psi);
        return new JSAttributeListStubImpl(namespace, flags, parentStub, this);
    }

    @Override
    public void serialize(@Nonnull JSAttributeListStub stub, @Nonnull StubOutputStream dataStream) throws IOException {
        dataStream.writeName(stub.getNamespace());
        dataStream.writeInt(stub.getFlags());
    }

    @Nonnull
    @Override
    public JSAttributeListStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException {
        StringRef namespaceRef = dataStream.readName();
        int i = dataStream.readInt();
        return new JSAttributeListStubImpl(StringRef.toString(namespaceRef), i, parentStub, this);
    }
}
