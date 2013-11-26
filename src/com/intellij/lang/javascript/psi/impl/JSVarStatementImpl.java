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
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import org.jetbrains.annotations.NotNull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:35:47 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSVarStatementImpl extends JSStubbedStatementImpl<JSVarStatementStub> implements JSVarStatement {


  public JSVarStatementImpl(final ASTNode node) {
    super(node);
  }

  public JSVarStatementImpl(final JSVarStatementStub node) {
    super(node, JSElementTypes.VAR_STATEMENT);
  }

  public JSVariable[] getVariables() {
    return getStubOrPsiChildren(JSElementTypes.VARIABLE, JSVariable.EMPTY_ARRAY);
  }

  public void declareVariable(String name, JSExpression initializer) {
    throw new UnsupportedOperationException("TODO: implement");
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof JSElementVisitor) {
      ((JSElementVisitor)visitor).visitJSVarStatement(this);
    }
    else {
      visitor.visitElement(this);
    }
  }

  public boolean processDeclarations(@NotNull PsiScopeProcessor processor,
                                     @NotNull ResolveState state,
                                     PsiElement lastParent,
                                     @NotNull PsiElement place) {
    final JSVariable[] vars = getVariables();
    
    for (JSVariable var : vars) {
      if (lastParent != null && lastParent.getParent() == var) break;
      if (!processor.execute(var, state)) return false;
    }


    return true;
  }
}
