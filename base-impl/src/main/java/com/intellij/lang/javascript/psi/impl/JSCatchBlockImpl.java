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
import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;

import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSCatchBlockImpl extends JSElementImpl implements JSCatchBlock {
    public JSCatchBlockImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public JSParameter getParameter() {
        ASTNode node = getNode().findChildByType(JSElementTypes.FORMAL_PARAMETER);
        return node != null ? (JSParameter)node.getPsi() : null;
    }

    @Override
    public JSStatement getStatement() {
        return findChildByClass(JSStatement.class);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSCatchBlock(this);
    }

    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        if (lastParent != null) {
            JSParameter param = getParameter();
            if (param != null) {
                return processor.execute(param, state);
            }
        }

        return true;
    }

    @Override
    public void delete() throws IncorrectOperationException {
        ASTNode astNode = getNode();
        astNode.getTreeParent().removeChild(astNode);
    }
}
