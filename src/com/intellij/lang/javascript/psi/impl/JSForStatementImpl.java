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
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import org.jetbrains.annotations.NotNull;

/**
 * @author max
 */
public class JSForStatementImpl extends JSStatementImpl implements JSForStatement {
  public JSForStatementImpl(final ASTNode node) {
    super(node);
  }

  public JSVarStatement getVarDeclaration() {
    final ASTNode node = getNode().findChildByType(JSElementTypes.VAR_STATEMENT);
    return (JSVarStatement)(node != null ? node.getPsi() : null);
  }

  public JSExpression getInitialization() {
    ASTNode child = getNode().getFirstChildNode();
    while (child != null) {
      if (child.getElementType() == JSTokenTypes.SEMICOLON) return null;
      if (JSElementTypes.EXPRESSIONS.contains(child.getElementType())) return (JSExpression)child.getPsi();
      child = child.getTreeNext();
    }
    return null;
  }

  public JSExpression getCondition() {
    ASTNode child = getNode().getFirstChildNode();
    int semicolonCount = 0;
    while (child != null) {
      if (child.getElementType() == JSTokenTypes.SEMICOLON) {
        semicolonCount++;
        if (semicolonCount == 2) return null;
      }
      else if (semicolonCount == 1 && JSElementTypes.EXPRESSIONS.contains(child.getElementType())) {
        return (JSExpression)child.getPsi();
      }
      child = child.getTreeNext();
    }
    return null;
  }

  public JSExpression getUpdate() {
    ASTNode child = getNode().getFirstChildNode();
    int semicolonCount = 0;
    while (child != null) {
      if (child.getElementType() == JSTokenTypes.SEMICOLON) {
        semicolonCount++;
      }
      else if (semicolonCount == 2 && JSElementTypes.EXPRESSIONS.contains(child.getElementType())) {
        return (JSExpression)child.getPsi();
      }
      child = child.getTreeNext();
    }
    return null;
  }

  public JSStatement getBody() {
    ASTNode child = getNode().getFirstChildNode();
    boolean passedRParen = false;
    while (child != null) {
      if (child.getElementType() == JSTokenTypes.RPAR) {
        passedRParen = true;
      }
      else if (passedRParen && JSElementTypes.STATEMENTS.contains(child.getElementType())) {
        return (JSStatement)child.getPsi();
      }
      child = child.getTreeNext();
    }

    return null;
  }

  public boolean processDeclarations(@NotNull PsiScopeProcessor processor,
                                     @NotNull ResolveState state,
                                     PsiElement lastParent,
                                     @NotNull PsiElement place) {
    if (lastParent != null) {
      final JSVarStatement statement = getVarDeclaration();
      if (statement != null) return statement.processDeclarations(processor, state, lastParent, place);
      else {
        final JSExpression initialization = getInitialization();
        if (initialization != null) {
          return initialization.processDeclarations(processor, state, null, place);
        }
      }
    }
    return true;
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof JSElementVisitor) {
      ((JSElementVisitor)visitor).visitJSForStatement(this);
    }
    else {
      visitor.visitElement(this);
    }
  }
}
