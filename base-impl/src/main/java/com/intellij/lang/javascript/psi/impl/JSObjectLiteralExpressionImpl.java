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

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.ResolveState;

import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSObjectLiteralExpressionImpl extends JSExpressionImpl implements JSObjectLiteralExpression {
    public JSObjectLiteralExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    public JSProperty[] getProperties() {
        return findChildrenByClass(JSProperty.class);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSObjectLiteralExpression(this);
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        if (lastParent == null || !(place instanceof JSProperty)) {
            return true;
        }
        JSProperty[] props = getProperties();

        for (JSProperty property : props) {
            if (!processor.execute(property, state)) {
                return false;
            }
        }

        return true;
    }
}
