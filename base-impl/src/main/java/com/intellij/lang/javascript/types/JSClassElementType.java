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

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.impl.JSClassImpl;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import com.intellij.lang.javascript.psi.stubs.impl.JSClassStubImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.index.io.StringRef;
import consulo.javascript.types.JSQualifiedStubElementType;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.IndexSink;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;
import consulo.util.lang.StringUtil;
import jakarta.annotation.Nonnull;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
 * @since 10:30:14 PM Mar 25, 2008
 */
public class JSClassElementType extends JSQualifiedStubElementType<JSClassStub, JSClass> {
    public JSClassElementType() {
        super("CLASS");
    }

    @Override
    public void indexStub(@Nonnull JSClassStub stub, @Nonnull IndexSink sink) {
        super.indexStub(stub, sink);

        String name = stub.getName();
        if (!StringUtil.isEmpty(name)) {
            sink.occurrence(JavaScriptIndexKeys.CLASSES_BY_NAME, name);
        }
    }

    @Nonnull
    @Override
    public PsiElement createElement(@Nonnull ASTNode astNode) {
        return new JSClassImpl(astNode);
    }

    @Override
    public JSClass createPsi(@Nonnull JSClassStub stub) {
        return new JSClassImpl(stub, this);
    }

    @RequiredReadAction
    @Override
    public JSClassStub createStub(@Nonnull JSClass psi, StubElement parentStub) {
        String name = psi.getName();
        int flags = JSClassStubImpl.getFlags(psi);
        String qualifiedName = psi.getQualifiedName();
        return new JSClassStubImpl(name, flags, qualifiedName, parentStub, this);
    }

    @Override
    public void serialize(@Nonnull JSClassStub stub, @Nonnull StubOutputStream dataStream) throws IOException {
        dataStream.writeName(stub.getName());
        dataStream.writeName(stub.getQualifiedName());
        dataStream.writeInt(stub.getFlags());
    }

    @Nonnull
    @Override
    public JSClassStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException {
        StringRef nameRef = dataStream.readName();
        StringRef qualifiedRef = dataStream.readName();
        int flags = dataStream.readInt();
        return new JSClassStubImpl(StringRef.toString(nameRef), flags, StringRef.toString(qualifiedRef), parentStub, this);
    }
}
