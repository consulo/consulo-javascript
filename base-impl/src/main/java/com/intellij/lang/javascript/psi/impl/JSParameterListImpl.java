/*
 * Copyright 2000-2005 JetBrains s.r.o.
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

package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.stubs.JSParameterListStub;
import consulo.language.ast.ASTNode;

import jakarta.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 8:41:53 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSParameterListImpl extends JSStubElementImpl<JSParameterListStub> implements JSParameterList {
    public JSParameterListImpl(final ASTNode node) {
        super(node);
    }

    public JSParameterListImpl(final JSParameterListStub stub) {
        super(stub, JSElementTypes.PARAMETER_LIST);
    }

    @Override
    public JSParameter[] getParameters() {
        return getStubOrPsiChildren(JSElementTypes.PARAMETERS, JSParameter.EMPTY_ARRAY);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSParameterList(this);
    }
}
