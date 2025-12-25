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
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.impl.JSVarStatementImpl;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSVarStatementStubImpl;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.StubElement;
import consulo.language.psi.stub.StubInputStream;
import consulo.language.psi.stub.StubOutputStream;

/**
 * @author Maxim.Mossienko
 * Date: Jun 8, 2008
 * Time: 1:50:59 PM
 */
public class JSVarStatementElementType extends JSStubElementType<JSVarStatementStub, JSVarStatement> {
    public JSVarStatementElementType() {
        super("VAR_STATEMENT");
    }

    @Override
    public boolean shouldCreateStub(ASTNode node) {
        PsiElement element = node.getTreeParent().getPsi();
        return element instanceof JSClass || element instanceof JSPackageStatement || element instanceof JSFile;
    }

    @Nonnull
    @Override
    public PsiElement createElement(@Nonnull ASTNode astNode) {
        return new JSVarStatementImpl(astNode);
    }

    @Override
    public JSVarStatement createPsi(@Nonnull JSVarStatementStub stub) {
        return new JSVarStatementImpl(stub);
    }

    @RequiredReadAction
    @Override
    public JSVarStatementStub createStub(@Nonnull JSVarStatement psi, StubElement parentStub) {
        return new JSVarStatementStubImpl(parentStub, this);
    }

    @Override
    public void serialize(@Nonnull JSVarStatementStub stub, @Nonnull StubOutputStream dataStream) throws IOException {

    }

    @Nonnull
    @Override
    public JSVarStatementStub deserialize(@Nonnull StubInputStream dataStream, StubElement parentStub) throws IOException {
        return new JSVarStatementStubImpl(parentStub, this);
    }
}
