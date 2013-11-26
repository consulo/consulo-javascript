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

import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;
import com.intellij.icons.AllIcons;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSNamespaceDeclaration;
import com.intellij.lang.javascript.psi.stubs.JSNamespaceDeclarationStub;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.ResolveState;
import com.intellij.psi.scope.PsiScopeProcessor;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;

/**
 * @by Maxim.Mossienko
 */
public class JSNamespaceDeclarationImpl extends JSStubbedStatementImpl<JSNamespaceDeclarationStub> implements JSNamespaceDeclaration {
  public JSNamespaceDeclarationImpl(final ASTNode node) {
    super(node);
  }

  public JSNamespaceDeclarationImpl(final JSNamespaceDeclarationStub node) {
    super(node, JSElementTypes.NAMESPACE_DECLARATION);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof JSElementVisitor) {
      ((JSElementVisitor)visitor).visitJSNamespaceDeclaration(this);
    }
    else {
      visitor.visitElement(this);
    }
  }

  public JSAttributeList getAttributeList() {
    return getStubOrPsiChild(JSElementTypes.ATTRIBUTE_LIST);
  }

  public PsiElement setName(@NonNls @NotNull String newName) throws IncorrectOperationException {
    final String oldName = getName();
    if(newName.equals(oldName)) return this;

    getNode().replaceChild(findNameIdentifier(), JSChangeUtil.createExpressionFromText(getProject(), newName));

    JSPsiImplUtils.updateFileName(this, newName, oldName);
    return this;
  }

  public String getName() {
    final JSNamespaceDeclarationStub stub = getStub();
    if (stub != null) return stub.getName();
    final ASTNode node = findNameIdentifier();
    return node != null? node.getText() : null;
  }

  public int getTextOffset() {
    final ASTNode node = findNameIdentifier();
    return node == null ? super.getTextOffset() : node.getStartOffset();
  }

  public ASTNode findNameIdentifier() {
    return getNode().findChildByType(JSElementTypes.REFERENCE_EXPRESSION);
  }

  public String getQualifiedName() {
    final JSNamespaceDeclarationStub stub = getStub();
    if (stub != null) return stub.getQualifiedName();
    return JSPsiImplUtils.getQName(this);
  }

  public PsiElement getNameIdentifier() {
    final ASTNode node = findNameIdentifier();
    return node != null ? node.getPsi():null;
  }

  public String getInitialValueString() {
    final JSNamespaceDeclarationStub stub = getStub();
    if (stub != null) return stub.getInitialValueString();
    final ASTNode anchor = getNode().findChildByType(JSTokenTypes.EQ);

    if (anchor != null) {
      ASTNode node = anchor.getTreeNext();

      if (node != null && node.getElementType() == JSTokenTypes.WHITE_SPACE) {
        node = node.getTreeNext();
      }

      IElementType type;
      if (node != null && ((type = node.getElementType()) == JSTokenTypes.STRING_LITERAL || type == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL)) {
        return node.getText();
      }
    }
    return null;
  }

  public boolean isDeprecated() {
    return false;
  }

  @Override
  public boolean processDeclarations(@NotNull final PsiScopeProcessor processor, @NotNull final ResolveState state, final PsiElement lastParent,
                                     @NotNull final PsiElement place) {
    return processor.execute(lastParent, state);
  }

  public Icon getIcon(int flags) {
    final JSAttributeList attributeList = getAttributeList();
    final JSAttributeList.AccessType type = attributeList != null ? attributeList.getAccessType(): JSAttributeList.AccessType.PACKAGE_LOCAL;
    return buildIcon(blendModifierFlags(AllIcons.Nodes.Class, attributeList),type.getIcon());
  }
}
