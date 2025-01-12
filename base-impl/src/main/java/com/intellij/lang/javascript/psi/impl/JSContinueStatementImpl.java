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

import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.util.PsiTreeUtil;

import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSContinueStatementImpl extends JSStatementWithLabelReferenceImpl implements JSContinueStatement {
    public JSContinueStatementImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public JSStatement getStatementToContinue() {
        String label = getLabel();
        if (label == null) {
            return PsiTreeUtil.getParentOfType(this, JSLoopStatement.class, JSSwitchStatement.class);
        }
        else {
            return (JSStatement)getReferences()[0].resolve();
        }
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSContinueStatement(this);
    }
}
