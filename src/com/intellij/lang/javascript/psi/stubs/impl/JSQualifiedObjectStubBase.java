package com.intellij.lang.javascript.psi.stubs.impl;

import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.lang.javascript.psi.stubs.JSNameIndex;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedElementIndex;
import com.intellij.lang.javascript.psi.stubs.JSQualifiedStub;
import com.intellij.psi.stubs.*;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 7:11:48 PM
 */
abstract class JSQualifiedObjectStubBase<T extends JSQualifiedNamedElement> extends JSNamedObjectStubBase<T> implements JSQualifiedStub<T> {
  protected final String myQualifiedName;

  protected JSQualifiedObjectStubBase(T clazz, final StubElement parent, final IStubElementType elementType) {
    super(clazz, parent, elementType);

    myQualifiedName = doGetQualifiedName(clazz);
  }

  protected JSQualifiedObjectStubBase(String name, int flags, String qName, final StubElement parent, final IStubElementType elementType) {
    super(name, flags, parent, elementType);

    myQualifiedName = qName;
  }

  protected String doGetQualifiedName(final T clazz) {
    return clazz.getQualifiedName();
  }

  public void index(final IndexSink sink) {
    final String name = getName();
    final String fqn = getQualifiedName();

    if (name != null && doIndexName(name, fqn)) {
      sink.occurrence(JSNameIndex.KEY, name);
    }

    if (fqn != null && doIndexQualifiedName(name, fqn)) {
      sink.occurrence(JSQualifiedElementIndex.KEY, fqn.hashCode());
    }
  }

  protected boolean doIndexQualifiedName(final String name, final String fqn) {
    return true;
  }

  protected boolean doIndexName(final String name, final String fqn) {
    return true;
  }

  protected JSQualifiedObjectStubBase(final StubInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws IOException {
    super(dataStream, parentStub, elementType);

    myQualifiedName = readString(dataStream);
  }

  public void serialize(final StubOutputStream dataStream) throws IOException {
    super.serialize(dataStream);
    writeString(myQualifiedName, dataStream);
  }

  public String getQualifiedName() {
    return myQualifiedName;
  }
}