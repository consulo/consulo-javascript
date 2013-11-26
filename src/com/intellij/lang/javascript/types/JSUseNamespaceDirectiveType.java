package com.intellij.lang.javascript.types;

import com.intellij.lang.javascript.psi.stubs.JSUseNamespaceDirectiveStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSUseNamespaceDirectiveStubImpl;
import com.intellij.lang.javascript.psi.JSUseNamespaceDirective;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubElement;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
*         Date: Oct 3, 2008
*         Time: 9:13:01 PM
*/
public class JSUseNamespaceDirectiveType extends JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> {
  private static JSStubGenerator<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> ourStubGenerator = new JSStubGenerator<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective>() {
    @Override
    public JSUseNamespaceDirectiveStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
                                                   final JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> type)
        throws IOException {
      return new JSUseNamespaceDirectiveStubImpl(dataStream, parentStub, type);
    }

    @Override
    public JSUseNamespaceDirectiveStub newInstance(final JSUseNamespaceDirective psi, final StubElement parentStub,
                                                   final JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> type) {
      return new JSUseNamespaceDirectiveStubImpl(psi, parentStub, type);
    }
  };

  public JSUseNamespaceDirectiveType() {
    super("USE_NAMESPACE_DIRECTIVE", ourStubGenerator);
  }
}
