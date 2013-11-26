package com.intellij.lang.javascript.types;

import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSParameterListStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSParameterListStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.util.io.PersistentStringEnumerator;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
*         Date: Mar 25, 2008
*         Time: 10:30:00 PM
*/
public class JSParameterListElementType extends JSStubElementType<JSParameterListStub, JSParameterList> {
  private static final JSStubGenerator<JSParameterListStub,JSParameterList> ourStubGenerator = new JSStubGenerator<JSParameterListStub, JSParameterList>() {
    public JSParameterListStub newInstance(final StubInputStream dataStream,
                                           final StubElement parentStub, final JSStubElementType<JSParameterListStub, JSParameterList> elementType) throws IOException {
      return new JSParameterListStubImpl(dataStream, parentStub, elementType);
    }

    public JSParameterListStub newInstance(final JSParameterList psi,
                                           final StubElement parentStub, final JSStubElementType<JSParameterListStub, JSParameterList> elementType) {
      return new JSParameterListStubImpl(psi, parentStub, elementType);
    }
  };

  public JSParameterListElementType() {
    super("PARAMETER_LIST", ourStubGenerator);
  }
}
