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
import consulo.content.scope.SearchScope;
import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.util.IncorrectOperationException;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.PsiElement;
import consulo.util.lang.Comparing;

import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSFunctionImpl extends JSFunctionBaseImpl<JSFunctionStub, JSFunction> implements JSSuppressionHolder {
    public JSFunctionImpl(ASTNode node) {
        super(node);
    }

    public JSFunctionImpl(JSFunctionStub stub, IStubElementType type) {
        super(stub, type);
    }

    @Override
    @RequiredReadAction
    public boolean isGetProperty() {
        JSFunctionStub stub = getStub();
        if (stub != null) {
            return stub.isGetProperty();
        }
        return findChildByType(JSTokenTypes.GET_KEYWORD) != null;
    }

    @Override
    @RequiredReadAction
    public boolean isSetProperty() {
        JSFunctionStub stub = getStub();
        if (stub != null) {
            return stub.isGetProperty();
        }
        return findChildByType(JSTokenTypes.SET_KEYWORD) != null;
    }

    @Override
    @RequiredReadAction
    public boolean isConstructor() {
        JSFunctionStub stub = getStub();
        if (stub != null) {
            return stub.isConstructor();
        }
        PsiElement parent = JSResolveUtil.findParent(this);
        return parent instanceof JSClass && Comparing.equal("constructor", getName(), true);
    }

    @Override
    public JSAttributeList getAttributeList() {
        return getStubOrPsiChild(JSElementTypes.ATTRIBUTE_LIST);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSFunctionDeclaration(this);
    }

    @Override
    @RequiredWriteAction
    public void delete() throws IncorrectOperationException {
        getNode().getTreeParent().removeChild(getNode());
    }

    @Override
    @RequiredReadAction
    public String getQualifiedName() {
        JSFunctionStub jsFunctionStub = getStub();
        if (jsFunctionStub != null) {
            return jsFunctionStub.getQualifiedName();
        }
        PsiElement parent = JSResolveUtil.findParent(this);

        if (parent instanceof JSFile || parent instanceof JSPackageStatement) {
            return JSPsiImplUtils.getQName(this);
        }
        else {
            return getName();
        }
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public SearchScope getUseScope() {
        if (isConstructor()) {
            return super.getUseScope();
        }
        return JSResolveUtil.findUseScope(this);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public PsiElement getNavigationElement() {
        if (getParent() instanceof JSClass jsClass) {
            PsiElement parentOriginalElement = jsClass.getNavigationElement();

            if (parentOriginalElement != jsClass) {
                JSFunction functionByNameAndKind = ((JSClass)parentOriginalElement).findFunctionByNameAndKind(getName(), getKind());
                return functionByNameAndKind != null ? functionByNameAndKind : this;
            }
        }
        return JSPsiImplUtils.findTopLevelNavigatableElement(this);
    }

    @Override
    @RequiredWriteAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        String oldName = getName();
        PsiElement element = super.setName(name);
        if (getParent() instanceof JSPackageStatement) {
            JSPsiImplUtils.updateFileName(this, name, oldName);
        }
        return element;
    }
}
