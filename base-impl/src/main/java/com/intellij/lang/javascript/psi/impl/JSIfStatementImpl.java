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

import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSIfStatementImpl extends JSStatementImpl implements JSIfStatement {
    public JSIfStatementImpl(ASTNode node) {
        super(node);
    }

    @Override
    public JSExpression getCondition() {
        return findChildByClass(JSExpression.class);
    }

    @Override
    public JSStatement getThen() {
        return findChildByClass(JSStatement.class);
    }

    @Override
    @RequiredReadAction
    public JSStatement getElse() {
        ASTNode myNode = getNode();
        ASTNode elseNode = myNode.findChildByType(JSTokenTypes.ELSE_KEYWORD);
        ASTNode node = elseNode != null ? myNode.findChildByType(JSElementTypes.STATEMENTS, elseNode) : null;
        return node != null ? (JSStatement)node.getPsi() : null;
    }

    @Override
    public void setThen(JSStatement statement) {
        throw new UnsupportedOperationException("TODO: implement");
    }

    @Override
    public void setElse(JSStatement statement) {
        throw new UnsupportedOperationException("TODO: implement");
    }

    @Override
    public void setCondition(JSExpression expr) {
        throw new UnsupportedOperationException("TODO: implement");
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSIfStatement(this);
    }
}
