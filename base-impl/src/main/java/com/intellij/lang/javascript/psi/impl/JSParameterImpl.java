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

import com.intellij.javascript.documentation.JSDocumentationUtils;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.stubs.JSParameterStub;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSParameterImpl extends JSVariableBaseImpl<JSParameterStub, JSParameter> implements JSParameter {
    public JSParameterImpl(ASTNode node) {
        super(node);
    }

    public JSParameterImpl(JSParameterStub stub) {
        super(stub, JSElementTypes.FORMAL_PARAMETER);
    }

    @Override
    @RequiredReadAction
    public JSFunction getDeclaringFunction() {
        return (JSFunction)getNode().getTreeParent().getTreeParent().getPsi();
    }

    @Override
    @RequiredReadAction
    public boolean isRest() {
        JSParameterStub parameterStub = getStub();
        return parameterStub != null ? parameterStub.isRest() : getRestElement() != null;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public PsiElement getRestElement() {
        return findChildByType(JSTokenTypes.DOT_DOT_DOT);
    }

    @Override
    @RequiredReadAction
    public boolean isOptional() {
        JSParameterStub parameterStub = getStub();
        if (parameterStub != null) {
            return parameterStub.isOptional();
        }
        return getInitializer() != null || JSDocumentationUtils.findOptionalStatusFromComments(this);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSParameter(this);
    }

    @Override
    public JSAttributeList getAttributeList() {
        return null;
    }

    @Override
    @RequiredWriteAction
    public void delete() throws IncorrectOperationException {
        ASTNode myNode = getNode();
        ASTNode parent = myNode.getTreeParent();

        if (parent.getElementType() == JSElementTypes.PARAMETER_LIST) {
            JSChangeUtil.removeRangeWithRemovalOfCommas(myNode, parent);
            return;
        }

        throw new IncorrectOperationException("Cannot delete variable from parent : " + parent.getElementType());
    }

    @Override
    @RequiredReadAction
    protected String doGetType() {
        String s = super.doGetType();

        if (s == null) {
            ASTNode astNode = getNode();
            ASTNode anchor = astNode.findChildByType(JSTokenTypes.INSTANCEOF_KEYWORD);

            if (anchor != null) {
                ASTNode type = astNode.findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET, anchor);
                if (type != null) {
                    s = type.getText();
                }
            }
        }
        return s;
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        return processor.execute(this, state);
    }
}
