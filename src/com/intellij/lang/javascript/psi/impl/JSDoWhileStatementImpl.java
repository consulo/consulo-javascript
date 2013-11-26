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
import com.intellij.lang.javascript.psi.JSDoWhileStatement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.psi.PsiElementVisitor;
import org.jetbrains.annotations.NotNull;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 10:15:13 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSDoWhileStatementImpl extends JSStatementImpl implements JSDoWhileStatement {
  public JSDoWhileStatementImpl(final ASTNode node) {
    super(node);
  }

  public JSExpression getCondition() {
    final ASTNode node = getNode().findChildByType(JSElementTypes.EXPRESSIONS);
    return node != null ? (JSExpression)node.getPsi() : null;
  }

  public JSStatement getBody() {
    final ASTNode node = getNode().findChildByType(JSElementTypes.STATEMENTS);
    return node != null ? (JSStatement)node.getPsi() : null;
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof JSElementVisitor) {
      ((JSElementVisitor)visitor).visitJSDoWhileStatement(this);
    }
    else {
      visitor.visitElement(this);
    }
  }
}
