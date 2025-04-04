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

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.language.psi.PsiElement;
import consulo.language.ast.ASTNode;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;

/**
 * @author max
 * @since 2005-01-30
 */
public class JSVariableImpl extends JSVariableBaseImpl<JSVariableStub, JSVariable> {
    public JSVariableImpl(ASTNode node) {
        super(node);
    }

    public JSVariableImpl(JSVariableStub stub) {
        super(stub, JSElementTypes.VARIABLE);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public PsiElement getNavigationElement() {
        if (getParent().getParent() instanceof JSClass jsClass) {
            PsiElement parentOriginalElement = jsClass.getNavigationElement();

            if (parentOriginalElement != jsClass) {
                JSVariable jsVariable = ((JSClass)parentOriginalElement).findFieldByName(getName());
                return jsVariable != null ? jsVariable : this;
            }
        }
        return JSPsiImplUtils.findTopLevelNavigatableElement(this);
    }

    @Override
    @RequiredWriteAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        String oldName = getName();
        PsiElement element = super.setName(name);
        if (getParent().getParent() instanceof JSPackageStatement) {
            JSPsiImplUtils.updateFileName(this, name, oldName);
        }
        return element;
    }
}
