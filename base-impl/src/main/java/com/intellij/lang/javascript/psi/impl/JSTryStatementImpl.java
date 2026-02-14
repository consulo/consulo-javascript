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
import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSTryStatementImpl extends JSStatementImpl implements JSTryStatement {
    private static final TokenSet CATCHES_TYPE_SET = TokenSet.create(JSElementTypes.CATCH_BLOCK);

    public JSTryStatementImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public JSStatement getStatement() {
        ASTNode child = getNode().getFirstChildNode();
        while (child != null) {
            IElementType type = child.getElementType();
            if (child.getPsi() instanceof JSStatement statement) {
                return statement;
            }
            if (type == JSTokenTypes.FINALLY_KEYWORD) {
                break;
            }
            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    @Nullable
    @RequiredReadAction
    public JSCatchBlock getCatchBlock() {
        ASTNode catchChild = getNode().findChildByType(JSElementTypes.CATCH_BLOCK);
        if (catchChild == null) {
            return null;
        }
        return (JSCatchBlock)catchChild.getPsi();
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public JSCatchBlock[] getAllCatchBlocks() {
        return findChildrenByClass(JSCatchBlock.class);
    }

    @Override
    @RequiredReadAction
    public JSStatement getFinallyStatement() {
        ASTNode child = getNode().getFirstChildNode();
        boolean foundFinally = false;
        while (child != null) {
            IElementType type = child.getElementType();
            if (foundFinally && child.getPsi() instanceof JSStatement statement) {
                return statement;
            }
            if (type == JSTokenTypes.FINALLY_KEYWORD) {
                foundFinally = true;
            }
            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSTryStatement(this);
    }
}
