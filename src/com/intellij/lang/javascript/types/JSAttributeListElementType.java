package com.intellij.lang.javascript.types;

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSAttributeListStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSAttributeListStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.util.io.PersistentStringEnumerator;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
*         Date: Mar 25, 2008
*         Time: 10:30:17 PM
*/
public class JSAttributeListElementType extends JSStubElementType<JSAttributeListStub, JSAttributeList> {
  private static final JSStubGenerator<JSAttributeListStub,JSAttributeList> ourStubGenerator = new JSStubGenerator<JSAttributeListStub, JSAttributeList>() {
    public JSAttributeListStub newInstance(final StubInputStream dataStream,
                                           final StubElement parentStub, final JSStubElementType<JSAttributeListStub, JSAttributeList> elementType) throws IOException {
      return new JSAttributeListStubImpl(dataStream, parentStub, elementType);
    }

    public JSAttributeListStub newInstance(final JSAttributeList psi,
                                           final StubElement parentStub, final JSStubElementType<JSAttributeListStub, JSAttributeList> elementType) {
      return new JSAttributeListStubImpl(psi, parentStub, elementType);
    }
  };

  public JSAttributeListElementType() {
    super("ATTRIBUTE_LIST", ourStubGenerator);
  }
}
