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
import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:52:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSBreakStatementImpl extends JSStatementWithLabelReferenceImpl implements JSBreakStatement {
    public JSBreakStatementImpl(final ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSBreakStatement(this);
    }

    @Override
    public JSStatement getStatementToBreak() {
        String label = getLabel();
        if (label == null) {
            return PsiTreeUtil.getParentOfType(this, JSLoopStatement.class, JSSwitchStatement.class);
        }
        else {
            return (JSStatement)getReferences()[0].resolve();
        }
    }
}
