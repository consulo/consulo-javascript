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
import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.util.IncorrectOperationException;
import org.jetbrains.annotations.NotNull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:17:15 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSBlockStatementImpl extends JSStatementImpl implements JSBlockStatement {
  public JSBlockStatementImpl(final ASTNode node) {
    super(node);
  }

  public JSStatement[] getStatements() {
    final ASTNode[] nodes = getNode().getChildren(JSElementTypes.STATEMENTS);
    final JSStatement[] statements = new JSStatement[nodes.length];
    for (int i = 0; i < statements.length; i++) {
      statements[i] = (JSStatement)nodes[i].getPsi();
    }
    return statements;
  }

  public JSLabeledStatement setLabel(String label) {
    throw new UnsupportedOperationException("TODO: implement");
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof JSElementVisitor) {
      ((JSElementVisitor)visitor).visitJSBlock(this);
    }
    else {
      visitor.visitElement(this);
    }
  }

  @Override
  public PsiElement add(@NotNull final PsiElement element) throws IncorrectOperationException {
    if (element instanceof JSStatement) {
      ASTNode node = getNode().findChildByType(JSTokenTypes.RBRACE);
      if (node != null) {
        PsiElement psiElement = super.addAfter(element, node.getTreePrev().getPsi());
        CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(getNode(), psiElement.getNode());
        return psiElement;
      }
    }
    return super.add(element);
  }
}
