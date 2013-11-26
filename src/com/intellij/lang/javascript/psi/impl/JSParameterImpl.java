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

import javax.swing.Icon;

import org.jetbrains.annotations.NotNull;
import com.intellij.icons.AllIcons;
import com.intellij.javascript.documentation.JSDocumentationUtils;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.stubs.JSParameterStub;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.util.IncorrectOperationException;

/**
 * Created by IntelliJ IDEA.
 * User: max
 * Date: Jan 30, 2005
 * Time: 9:12:51 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSParameterImpl extends JSVariableBaseImpl<JSParameterStub, JSParameter> implements JSParameter {
  public JSParameterImpl(final ASTNode node) {
    super(node);
  }

  public JSParameterImpl(final JSParameterStub stub) {
    super(stub, JSElementTypes.FORMAL_PARAMETER);
  }

  public JSFunction getDeclaringFunction() {
    return (JSFunction)getNode().getTreeParent().getTreeParent().getPsi();
  }

  public boolean isRest() {
    final JSParameterStub parameterStub = getStub();
    if (parameterStub != null) return parameterStub.isRest();
    return getNode().findChildByType(JSTokenTypes.DOT_DOT_DOT) != null;
  }

  public boolean isOptional() {
    final JSParameterStub parameterStub = getStub();
    if (parameterStub != null) return parameterStub.isOptional();
    if (getInitializer() != null) return true;

    return JSDocumentationUtils.findOptionalStatusFromComments(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof JSElementVisitor) {
      ((JSElementVisitor)visitor).visitJSParameter(this);
    }
    else {
      visitor.visitElement(this);
    }
  }

  public Icon getIcon(int flags) {
    return AllIcons.Nodes.Parameter;
  }

  public JSAttributeList getAttributeList() {
    return null;
  }

  public void delete() throws IncorrectOperationException {
    final ASTNode myNode = getNode();
    final ASTNode parent = myNode.getTreeParent();

    if (parent.getElementType() == JSElementTypes.PARAMETER_LIST) {
      JSChangeUtil.removeRangeWithRemovalOfCommas(myNode, parent);
      return;
    }

    throw new IncorrectOperationException("Cannot delete variable from parent : " + parent.getElementType());
  }

  protected String doGetType() {
    String s = super.doGetType();
    
    if (s == null) {
      final ASTNode astNode = getNode();
      final ASTNode anchor = astNode.findChildByType(JSTokenTypes.INSTANCEOF_KEYWORD);

      if (anchor != null) {
        ASTNode type = astNode.findChildByType(JSTokenTypes.IDENTIFIER_TOKENS_SET, anchor);
        if (type != null) {
          s = type.getText();
        }
      }
    }
    return s;
  }
}
