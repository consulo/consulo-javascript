package com.intellij.lang.javascript.types;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSVariableStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.tree.IElementType;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
*         Date: Mar 25, 2008
*         Time: 10:30:10 PM
*/
public class JSVariableElementType extends JSStubElementType<JSVariableStub, JSVariable> {
  private static final JSStubGenerator<JSVariableStub,JSVariable> ourStubGenerator = new JSStubGenerator<JSVariableStub, JSVariable>() {
    public JSVariableStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
                                      final JSStubElementType<JSVariableStub, JSVariable> elementType) throws IOException {
      return new JSVariableStubImpl(dataStream, parentStub, elementType);
    }

    public JSVariableStub newInstance(final JSVariable psi, final StubElement parentStub, final JSStubElementType<JSVariableStub, JSVariable> elementType) {
      return new JSVariableStubImpl(psi, parentStub, elementType);
    }
  };

  public JSVariableElementType() {
    super("VARIABLE", ourStubGenerator);
  }

  @Override
  public boolean shouldCreateStub(final ASTNode node) {
    final IElementType discriminatingParentType = node.getTreeParent().getTreeParent().getElementType();
    return discriminatingParentType == JSElementTypes.PACKAGE_STATEMENT ||
           discriminatingParentType == JSElementTypes.CLASS || 
           discriminatingParentType instanceof JSFileElementType;
  }
}
