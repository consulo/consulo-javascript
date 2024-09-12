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
import consulo.language.psi.PsiElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.lang.psi.impl.JavaScriptClassType;

import jakarta.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:57:36 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSNewExpressionImpl extends JSExpressionImpl implements JSNewExpression {
    public JSNewExpressionImpl(final ASTNode node) {
        super(node);
    }

    @RequiredReadAction
    @Nonnull
    @Override
    public JavaScriptType getType() {
        JSExpression methodExpression = getMethodExpression();

        return methodExpression instanceof JSReferenceExpression referenceExpression
            && referenceExpression.resolve() instanceof JSClass jsClass
            ? new JavaScriptClassType(jsClass)
            : super.getType();
    }

    @Override
    public JSExpression getMethodExpression() {
        return findChildByClass(JSExpression.class);
    }

    @Override
    public JSArgumentList getArgumentList() {
        final ASTNode node = getNode().findChildByType(JSElementTypes.ARGUMENT_LIST);
        return node != null ? (JSArgumentList)node.getPsi() : null;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSNewExpression(this);
    }
}
