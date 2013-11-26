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

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 8:47:58 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSVariableImpl extends JSVariableBaseImpl<JSVariableStub, JSVariable> {
  public JSVariableImpl(final ASTNode node) {
    super(node);
  }

  public JSVariableImpl(final JSVariableStub stub) {
    super(stub, JSElementTypes.VARIABLE);
  }

  @Override
  public PsiElement getNavigationElement() {
    PsiElement parent = getParent().getParent();
    if (parent instanceof JSClass) {
      PsiElement parentOriginalElement = parent.getNavigationElement();

      if (parentOriginalElement != parent) {
        JSVariable jsVariable = ((JSClass)parentOriginalElement).findFieldByName(getName());
        return jsVariable != null ? jsVariable:this;
      }
    }
    return JSPsiImplUtils.findTopLevelNavigatableElement(this);
  }

  @Override
  public PsiElement setName(@NotNull String name) throws IncorrectOperationException {
    String oldName = getName();
    PsiElement element = super.setName(name);
    if (getParent().getParent() instanceof JSPackageStatement) {
      JSPsiImplUtils.updateFileName(this, name, oldName);
    }
    return element;
  }
}
