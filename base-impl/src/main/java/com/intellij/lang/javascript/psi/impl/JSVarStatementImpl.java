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

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.annotation.access.RequiredReadAction;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:35:47 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSVarStatementImpl extends JSStubbedStatementImpl<JSVarStatementStub> implements JSVarStatement {
    public JSVarStatementImpl(final ASTNode node) {
        super(node);
    }

    public JSVarStatementImpl(final JSVarStatementStub node) {
        super(node, JSElementTypes.VAR_STATEMENT);
    }

    @Override
    public JSVariable[] getVariables() {
        JSDestructuringElement destructuringElement = getDestructuringElement();
        if (destructuringElement != null) {
            return destructuringElement.getVariables();
        }
        return getStubOrPsiChildren(JSElementTypes.VARIABLE, JSVariable.EMPTY_ARRAY);
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSDestructuringElement getDestructuringElement() {
        return findChildByClass(JSDestructuringElement.class);
    }

    @Override
    public void declareVariable(String name, JSExpression initializer) {
        throw new UnsupportedOperationException("TODO: implement");
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSVarStatement(this);
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        final JSVariable[] vars = getVariables();

        for (JSVariable var : vars) {
            if (lastParent != null && lastParent.getParent() == var) {
                break;
            }

            if (!processor.execute(var, state)) {
                return false;
            }
        }

        return true;
    }
}
