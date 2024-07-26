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
import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;

import jakarta.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 31, 2005
 * Time: 12:02:38 AM
 * To change this template use File | Settings | File Templates.
 */
public class JSCallExpressionImpl extends JSExpressionImpl implements JSCallExpression {
    public JSCallExpressionImpl(final ASTNode node) {
        super(node);
    }

    @Override
    public JSExpression getMethodExpression() {
        return findChildByClass(JSExpression.class);
    }

    @Override
    public JSArgumentList getArgumentList() {
        ASTNode node = getNode().findChildByType(JSElementTypes.ARGUMENT_LIST);
        return node != null ? (JSArgumentList)node.getPsi() : null;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSCallExpression(this);
    }
}
