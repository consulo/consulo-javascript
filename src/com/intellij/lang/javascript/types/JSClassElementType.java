package com.intellij.lang.javascript.types;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSClassStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.util.io.PersistentStringEnumerator;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
*         Date: Mar 25, 2008
*         Time: 10:30:14 PM
*/
public class JSClassElementType extends JSStubElementType<JSClassStub, JSClass> {
  private static final JSStubGenerator<JSClassStub,JSClass> ourStubGenerator = new JSStubGenerator<JSClassStub, JSClass>() {
    public JSClassStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
                                   final JSStubElementType<JSClassStub, JSClass> elementType) throws IOException {
      return new JSClassStubImpl(dataStream, parentStub, elementType);
    }

    public JSClassStub newInstance(final JSClass psi, final StubElement parentStub,
                                   final JSStubElementType<JSClassStub, JSClass> elementType) {
      return new JSClassStubImpl(psi, parentStub, elementType);
    }
};

  public JSClassElementType() {
    super("CLASS", ourStubGenerator);
  }
}
