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

import consulo.annotation.access.RequiredWriteAction;
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
 * @author max
 * @since 2005-01-30
 */
public class JSFunctionExpressionImpl extends JSFunctionBaseImpl<JSFunctionStub, JSFunctionExpression> implements JSFunctionExpression {
    public JSFunctionExpressionImpl(ASTNode node) {
        super(node);
    }

    public JSFunctionExpressionImpl(JSFunctionStub stub, IStubElementType type) {
        super(stub, type);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSFunctionExpression(this);
    }

    @Override
    @RequiredReadAction
    protected ASTNode createNameIdentifier(String name) {
        return JSChangeUtil.createNameIdentifier(getProject(), name);
    }

    @Nonnull
    @Override
    @RequiredWriteAction
    public JSExpression replace(JSExpression newExpr) {
        return JSChangeUtil.replaceExpression(this, newExpr);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public JavaScriptType getType() {
        return JavaScriptType.UNKNOWN;
    }

    @Override
    public JSAttributeList getAttributeList() {
        return null;
    }

    @Override
    @RequiredReadAction
    public int getTextOffset() {
        PsiElement name = getNameIdentifier();
        return name != null ? name.getTextOffset() : super.getTextOffset();
    }

    @Override
    public void delete() throws IncorrectOperationException {
        PsiElement parent = getParent();
        if (parent instanceof JSAssignmentExpression assignment) {
            assignment.getLOperand().delete();
            return;
        }
        super.delete();
    }

    @Override
    @RequiredReadAction
    public boolean isGetProperty() {
        return false;
    }

    @Override
    @RequiredReadAction
    public boolean isSetProperty() {
        return false;
    }

    @Override
    @RequiredReadAction
    public boolean isConstructor() {
        return false;
    }

    @Override
    @RequiredReadAction
    public String getQualifiedName() {
        return getName();
    }

    @RequiredReadAction
    @Override
    public PsiElement getNameIdentifier() {
        ASTNode treeParent = getNode().getTreeParent();
        PsiElement psi = treeParent != null ? treeParent.getPsi() : null;
        if (psi instanceof JSCallExpression call) {
            psi = call.getParent();
        }
        if (psi instanceof JSAssignmentExpression assignment) {
            JSExpression jsExpression = assignment.getLOperand();
            JSExpression lOperand = jsExpression instanceof JSDefinitionExpression definition ? definition.getExpression() : null;

            if (lOperand instanceof JSReferenceExpression refExpr) {
                ASTNode childByType = refExpr.getNode().findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET);
                return childByType != null ? childByType.getPsi() : null;
            }
        }
        else if (psi instanceof JSProperty property) {
            ASTNode childByType = property.getNode().findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET);
            return childByType != null ? childByType.getPsi() : null;
        }
        else {
            PsiElement node = super.getNameIdentifier();

            if (node != null) {
                return node;
            }

            if (psi instanceof JSVariable variable) {
                ASTNode childByType = variable.getNode().findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET);
                return childByType != null ? childByType.getPsi() : null;
            }
        }
        return null;
    }
}
