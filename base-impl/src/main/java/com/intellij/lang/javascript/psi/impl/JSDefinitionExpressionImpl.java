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
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.util.IncorrectOperationException;
import consulo.language.ast.ASTNode;

import jakarta.annotation.Nonnull;

/**
 * @author maxim.mossienko
 * @since 2005-12-14
 */
public class JSDefinitionExpressionImpl extends JSExpressionImpl implements JSDefinitionExpression {
    public JSDefinitionExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    public JSExpression getExpression() {
        return findChildByClass(JSExpression.class);
    }

    @Override
    @RequiredReadAction
    public String getName() {
        return getExpression() instanceof JSReferenceExpression referenceExpression
            ? referenceExpression.getReferencedName()
            : null;
    }

    @Override
    @RequiredWriteAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        return getExpression() instanceof JSReferenceExpressionImpl referenceExpression
            ? referenceExpression.handleElementRenameInternal(name)
            : null;
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSDefinitionExpression(this);
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        return lastParent != null || processor.execute(this, state);
    }

    @Override
    @RequiredWriteAction
    public void delete() throws IncorrectOperationException {
        if (getParent() instanceof JSAssignmentExpression assignment) {
            PsiElement assignmentParent = assignment.getParent();

            if (assignmentParent instanceof JSStatement statement) {
                statement.delete();
                return;
            }
            else if (assignmentParent instanceof JSBinaryExpression binaryExpression) {
                binaryExpression.getROperand().replace(assignment.getROperand());
                return;
            }
            else if (assignmentParent instanceof JSVariable variable) {
                JSExpression initializer = variable.getInitializer();
                initializer.replace(assignment.getROperand());
                return;
            }
        }
        super.delete();
    }

    @Override
    @RequiredReadAction
    public PsiElement getNameIdentifier() {
        return null;
    }
}
