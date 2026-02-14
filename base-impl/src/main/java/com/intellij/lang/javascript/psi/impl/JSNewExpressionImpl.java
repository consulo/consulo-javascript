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
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.psi.impl.JavaScriptClassType;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSNewExpressionImpl extends JSExpressionImpl implements JSNewExpression {
    public JSNewExpressionImpl(ASTNode node) {
        super(node);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public JavaScriptType getType() {
        JSExpression methodExpression = getMethodExpression();

        if (methodExpression instanceof JSReferenceExpression referenceExpression) {
            PsiElement element = referenceExpression.resolve();
            if (element instanceof JSClass jsClass) {
                return new JavaScriptClassType(jsClass);
            }

            if (element instanceof JSVariable jsVar) {
                return jsVar.getType();
            }
        }
        return super.getType();
    }

    @Override
    public JSExpression getMethodExpression() {
        return findChildByClass(JSExpression.class);
    }

    @Override
    @RequiredReadAction
    public JSArgumentList getArgumentList() {
        ASTNode node = getNode().findChildByType(JSElementTypes.ARGUMENT_LIST);
        return node != null ? (JSArgumentList)node.getPsi() : null;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSNewExpression(this);
    }
}
