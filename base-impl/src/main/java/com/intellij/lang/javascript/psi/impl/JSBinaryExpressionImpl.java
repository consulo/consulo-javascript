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
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.ast.TokenSet;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.util.PsiTreeUtil;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSBinaryExpressionImpl extends JSExpressionImpl implements JSBinaryExpression {
    private static final TokenSet BINARY_OPERATIONS = TokenSet.orSet(JSTokenTypes.OPERATIONS, JSTokenTypes.RELATIONAL_OPERATIONS);
    private static final TokenSet BINARY_OPERATIONS_WITH_DEFS = TokenSet.create(JSTokenTypes.COMMA, JSTokenTypes.EQ);

    public JSBinaryExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public JSExpression getLOperand() {
        ASTNode astNode = getNode();
        JSExpression firstExpression = PsiTreeUtil.findChildOfType(astNode.getPsi(), JSExpression.class);
        if (firstExpression != null && astNode.findChildByType(BINARY_OPERATIONS, firstExpression.getNode()) == null) {
            return null; // =a
        }
        return firstExpression != null ? firstExpression : null;
    }

    @Override
    @RequiredReadAction
    public JSExpression getROperand() {
        ASTNode myNode = getNode();
        ASTNode secondExpression = myNode.findChildByType(JSElementTypes.EXPRESSIONS, myNode.findChildByType(BINARY_OPERATIONS));
        return secondExpression != null ? (JSExpression)secondExpression.getPsi() : null;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public PsiElement getOperationElement() {
        ASTNode operationASTNode = getNode().findChildByType(BINARY_OPERATIONS);
        return operationASTNode != null ? operationASTNode.getPsi() : null;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSBinaryExpression(this);
    }

    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        IElementType operationType = getOperationSign();

        if (BINARY_OPERATIONS_WITH_DEFS.contains(operationType)) {
            JSExpression loperand = getLOperand();
            JSExpression roperand = getROperand();

            if (loperand != null) {
                return loperand.processDeclarations(processor, state, lastParent, place)
                    && (roperand == null || roperand.processDeclarations(processor, state, lastParent, place));
            }
        }

        return true;
    }
}
