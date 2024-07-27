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
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.resolve.PsiScopeProcessor;

import jakarta.annotation.Nonnull;

/**
 * @author max
 */
public class JSForStatementImpl extends JSStatementImpl implements JSForStatement {
    public JSForStatementImpl(final ASTNode node) {
        super(node);
    }

    @Override
    public JSVarStatement getVarDeclaration() {
        final ASTNode node = getNode().findChildByType(JSElementTypes.VAR_STATEMENT);
        return (JSVarStatement)(node != null ? node.getPsi() : null);
    }

    @Override
    public JSExpression getInitialization() {
        ASTNode child = getNode().getFirstChildNode();
        while (child != null) {
            if (child.getElementType() == JSTokenTypes.SEMICOLON) {
                return null;
            }
            if (JSElementTypes.EXPRESSIONS.contains(child.getElementType())) {
                return (JSExpression)child.getPsi();
            }
            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    public JSExpression getCondition() {
        ASTNode child = getNode().getFirstChildNode();
        int semicolonCount = 0;
        while (child != null) {
            if (child.getElementType() == JSTokenTypes.SEMICOLON) {
                semicolonCount++;
                if (semicolonCount == 2) {
                    return null;
                }
            }
            else if (semicolonCount == 1 && JSElementTypes.EXPRESSIONS.contains(child.getElementType())) {
                return (JSExpression)child.getPsi();
            }
            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    public JSExpression getUpdate() {
        ASTNode child = getNode().getFirstChildNode();
        int semicolonCount = 0;
        while (child != null) {
            if (child.getElementType() == JSTokenTypes.SEMICOLON) {
                semicolonCount++;
            }
            else if (semicolonCount == 2 && JSElementTypes.EXPRESSIONS.contains(child.getElementType())) {
                return (JSExpression)child.getPsi();
            }
            child = child.getTreeNext();
        }
        return null;
    }

    @Override
    public JSStatement getBody() {
        ASTNode child = getNode().getFirstChildNode();
        boolean passedRParen = false;
        while (child != null) {
            if (child.getElementType() == JSTokenTypes.RPAR) {
                passedRParen = true;
            }
            else if (passedRParen && child.getPsi() instanceof JSStatement statement) {
                return statement;
            }
            child = child.getTreeNext();
        }

        return null;
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        if (lastParent != null) {
            final JSVarStatement statement = getVarDeclaration();
            if (statement != null) {
                return statement.processDeclarations(processor, state, lastParent, place);
            }
            else {
                final JSExpression initialization = getInitialization();
                if (initialization != null) {
                    return initialization.processDeclarations(processor, state, null, place);
                }
            }
        }
        return true;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSForStatement(this);
    }
}
