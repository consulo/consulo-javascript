package com.intellij.lang.javascript.psi.stubs.impl;

import com.intellij.lang.javascript.psi.JSNamespaceDeclaration;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSNamespaceDeclarationImpl;
import com.intellij.lang.javascript.psi.stubs.JSNamespaceDeclarationStub;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 8:00:52 PM
 */
public class JSNamespaceDeclarationStubImpl extends JSQualifiedObjectStubBase<JSNamespaceDeclaration> implements JSNamespaceDeclarationStub {
  private String myInitialValueString;

  public JSNamespaceDeclarationStubImpl(final StubInputStream dataStream, final StubElement parentStub,
                                        final JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> type) throws IOException {
    super(dataStream,parentStub, type);
    myInitialValueString = readString(dataStream);
  }

  public JSNamespaceDeclarationStubImpl(final JSNamespaceDeclaration psi,
                                        final StubElement parentStub,
                                        final JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> type) {
    super(psi, parentStub, type);
    myInitialValueString = psi.getInitialValueString();
  }

  public JSNamespaceDeclaration createPsi() {
    return new JSNamespaceDeclarationImpl(this);
  }

  protected int buildFlags(final JSNamespaceDeclaration clazz) {
    return 0;
  }

  @Override
  public void serialize(final StubOutputStream dataStream) throws IOException {
    super.serialize(dataStream);
    writeString(myInitialValueString, dataStream);
  }

  public String getInitialValueString() {
    return myInitialValueString;
  }
}
