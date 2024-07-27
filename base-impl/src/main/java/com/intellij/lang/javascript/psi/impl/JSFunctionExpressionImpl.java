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
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import consulo.language.psi.PsiElement;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.util.IncorrectOperationException;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;

import jakarta.annotation.Nonnull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 11:55:33 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSFunctionExpressionImpl extends JSFunctionBaseImpl<JSFunctionStub, JSFunctionExpression> implements JSFunctionExpression {
    public JSFunctionExpressionImpl(final ASTNode node) {
        super(node);
    }

    public JSFunctionExpressionImpl(final JSFunctionStub stub, IStubElementType type) {
        super(stub, type);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSFunctionExpression(this);
    }

    @Override
    protected ASTNode createNameIdentifier(final String name) {
        return JSChangeUtil.createNameIdentifier(getProject(), name);
    }

    @Override
    public JSExpression replace(JSExpression newExpr) {
        return JSChangeUtil.replaceExpression(this, newExpr);
    }

    @RequiredReadAction
    @Nonnull
    @Override
    public JavaScriptType getType() {
        return JavaScriptType.UNKNOWN;
    }

    @Override
    public JSAttributeList getAttributeList() {
        return null;
    }

    @RequiredReadAction
    @Override
    public int getTextOffset() {
        final PsiElement name = getNameIdentifier();
        return name != null ? name.getTextOffset() : super.getTextOffset();
    }

    @Override
    public void delete() throws IncorrectOperationException {
        final PsiElement parent = getParent();
        if (parent instanceof JSAssignmentExpression assignment) {
            assignment.getLOperand().delete();
            return;
        }
        super.delete();
    }

    @RequiredReadAction
    @Override
    public boolean isGetProperty() {
        return false;
    }

    @RequiredReadAction
    @Override
    public boolean isSetProperty() {
        return false;
    }

    @RequiredReadAction
    @Override
    public boolean isConstructor() {
        return false;
    }

    @Override
    public String getQualifiedName() {
        return getName();
    }

    @RequiredReadAction
    @Override
    public PsiElement getNameIdentifier() {
        final ASTNode treeParent = getNode().getTreeParent();
        PsiElement psi = treeParent != null ? treeParent.getPsi() : null;
        if (psi instanceof JSCallExpression) {
            psi = psi.getParent();
        }
        if (psi instanceof JSAssignmentExpression assignment) {
            final JSExpression jsExpression = assignment.getLOperand();
            final JSExpression lOperand = jsExpression instanceof JSDefinitionExpression definition ? definition.getExpression() : null;

            if (lOperand instanceof JSReferenceExpression) {
                ASTNode childByType = lOperand.getNode().findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET);
                return childByType != null ? childByType.getPsi() : null;
            }
        }
        else if (psi instanceof JSProperty) {
            ASTNode childByType = psi.getNode().findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET);
            return childByType != null ? childByType.getPsi() : null;
        }
        else {
            final PsiElement node = super.getNameIdentifier();

            if (node != null) {
                return node;
            }

            if (psi instanceof JSVariable) {
                ASTNode childByType = psi.getNode().findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET);
                return childByType != null ? childByType.getPsi() : null;
            }
        }
        return null;
    }
}
