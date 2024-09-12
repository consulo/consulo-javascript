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
import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.psi.JSAttribute;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSAttributeImpl;
import com.intellij.lang.javascript.psi.stubs.JSAttributeStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSAttributeStubImpl;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubOutputStream;
import consulo.index.io.StringRef;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.StubInputStream;

/**
 * @author Maxim.Mossienko
 * Date: Mar 25, 2008
 * Time: 10:30:20 PM
 */
public class JSAttributeElementType extends JSStubElementType<JSAttributeStub, JSAttribute> {
    public JSAttributeElementType() {
        super("ATTRIBUTE");
    }

    @Nonnull
    @Override
    public PsiElement createElement(@Nonnull ASTNode astNode) {
        return new JSAttributeImpl(astNode);
    }

    @Override
    public JSAttribute createPsi(@Nonnull JSAttributeStub stub) {
        return new JSAttributeImpl(stub);
    }

    @RequiredReadAction
    @Override
    public JSAttributeStub createStub(@Nonnull JSAttribute psi, StubElement parentStub) {
        return new JSAttributeStubImpl(psi.getName(), 0, parentStub);
    }

    @Override
    public void serialize(@Nonnull JSAttributeStub stub, @Nonnull StubOutputStream dataStream) throws IOException {
        dataStream.writeName(stub.getName());
    }

    @Nonnull
    @Override
    public JSAttributeStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException {
        StringRef name = dataStream.readName();
        return new JSAttributeStubImpl(StringRef.toString(name), 0, parentStub);
    }
}
