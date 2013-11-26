package com.intellij.lang.javascript.psi.stubs.impl;

import com.intellij.lang.javascript.psi.JSAttribute;
import com.intellij.lang.javascript.psi.impl.JSAttributeImpl;
import com.intellij.lang.javascript.psi.stubs.JSAttributeStub;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 11:29:19 PM
 */
public class JSAttributeStubImpl extends JSNamedObjectStubBase<JSAttribute> implements JSAttributeStub {
  public JSAttributeStubImpl(JSAttribute clazz, final StubElement parent, final IStubElementType elementType) {
    super(clazz, parent, elementType);
  }

  public JSAttributeStubImpl(final StubInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws IOException {
    super(dataStream, parentStub, elementType);
  }

  public JSAttributeStubImpl(final String name, final StubElement parentStub) {
    super(name, 0, parentStub, JSElementTypes.ATTRIBUTE);
  }

  protected int buildFlags(final JSAttribute clazz) {
    return 0;
  }

  public JSAttribute createPsi() {
    return new JSAttributeImpl(this);
  }

  public void index(final IndexSink sink) {
  }
}
