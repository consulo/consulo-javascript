package com.intellij.lang.javascript.psi.stubs.impl;

import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.impl.JSParameterListImpl;
import com.intellij.lang.javascript.psi.stubs.JSParameterListStub;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.psi.stubs.*;

import java.io.DataInputStream;
import java.io.IOException;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 11:29:19 PM
 */
public class JSParameterListStubImpl extends StubBase<JSParameterList> implements JSParameterListStub {
  public JSParameterListStubImpl(JSParameterList clazz, final StubElement parent, final IStubElementType elementType) {
    super(parent, elementType);
  }

  public JSParameterListStubImpl(final DataInputStream dataStream, final StubElement parentStub, final IStubElementType elementType)
      throws IOException {
    super(parentStub, elementType);
  }

  public JSParameterListStubImpl(final StubElement parentStub) {
    super(parentStub, JSElementTypes.PARAMETER_LIST);
  }

  public JSParameterList createPsi() {
    return new JSParameterListImpl(this);
  }

  public void index(final IndexSink sink) {
  }

  public void serialize(final StubOutputStream dataStream) throws IOException {
  }
}