package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSSuppressionHolder;
import com.intellij.lang.javascript.types.JSFileElementType;
import com.intellij.psi.codeStyle.CodeStyleManager;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.IncorrectOperationException;

/**
 * @author ven
 */
class JSStubbedStatementImpl<T extends StubElement> extends JSStubElementImpl<T> implements JSStatement, JSSuppressionHolder {
  JSStubbedStatementImpl(final ASTNode node) {
    super(node);
  }

  JSStubbedStatementImpl(final T t, IStubElementType type) {
    super(t, type);
  }

  public JSStatement addStatementBefore(JSStatement toAdd) throws IncorrectOperationException {
    return addStatementImpl(toAdd, true);
  }

  public JSStatement addStatementAfter(JSStatement toAdd) throws IncorrectOperationException {
    return addStatementImpl(toAdd, false);
  }

  //TODO: [lesya] the formatter stuff definitely needs more intelligence
  private JSStatement addStatementImpl(final JSStatement toAdd, final boolean before) throws IncorrectOperationException {
    final ASTNode treeParent = getNode().getTreeParent();

    if (treeParent.getElementType() != JSElementTypes.BLOCK_STATEMENT &&
        !(treeParent.getElementType() instanceof JSFileElementType) &&
        treeParent.getElementType() != JSElementTypes.CLASS &&
        treeParent.getElementType() != JSElementTypes.EMBEDDED_CONTENT
      ) {
      if (before) {
        return (JSStatement)treeParent.getPsi().addBefore(toAdd, this);
      }
      else {
        return (JSStatement)treeParent.getPsi().addAfter(toAdd, this);
      }
    } else {
      final ASTNode copy = toAdd.getNode().copyElement();
      addChildAndReformat(treeParent, copy, before ? getNode() : getNode().getTreeNext());
      return (JSStatement)copy.getPsi();
    }
  }

  private void addChildAndReformat(final ASTNode block, final ASTNode addedElement, final ASTNode anchorBefore) throws
                                                                                                                IncorrectOperationException {
    block.addChild(addedElement, anchorBefore);
    CodeStyleManager.getInstance(getProject()).reformatNewlyAddedElement(block, addedElement);
  }

  public JSStatement replace(JSStatement newStatement) {
    return JSChangeUtil.replaceStatement(this, newStatement);
  }

  public void delete() throws IncorrectOperationException {
    getNode().getTreeParent().removeChild(getNode());
  }
}