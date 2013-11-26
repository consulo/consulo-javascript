package com.intellij.lang.javascript.types;

import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSPackageStatementStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSPackageStatementStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.util.io.PersistentStringEnumerator;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:49:25 PM
 */
public class JSPackageStatementElementType extends JSStubElementType<JSPackageStatementStub, JSPackageStatement> {
  private static final JSStubGenerator<JSPackageStatementStub,JSPackageStatement> ourStubGenerator = new JSStubGenerator<JSPackageStatementStub, JSPackageStatement>() {
    public JSPackageStatementStub newInstance(final StubInputStream dataStream,
                                              final StubElement parentStub,
                                              final JSStubElementType<JSPackageStatementStub, JSPackageStatement> type) throws IOException {
      return new JSPackageStatementStubImpl(dataStream, parentStub, type);
    }

    public JSPackageStatementStub newInstance(final JSPackageStatement psi,
                                              final StubElement parentStub,
                                              final JSStubElementType<JSPackageStatementStub, JSPackageStatement> type) {
      return new JSPackageStatementStubImpl(psi, parentStub, type);
    }
  };

  public JSPackageStatementElementType() {
    super("PACKAGE_STATEMENT", ourStubGenerator);
  }
}
