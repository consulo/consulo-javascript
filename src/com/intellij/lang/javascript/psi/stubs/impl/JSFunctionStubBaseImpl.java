package com.intellij.lang.javascript.psi.stubs.impl;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStubBase;
import com.intellij.lang.javascript.types.JSPackageStatementElementType;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 7:11:48 PM
 */
abstract class JSFunctionStubBaseImpl<T extends JSFunction> extends JSQualifiedObjectStubBase<T> implements JSFunctionStubBase<T> {
  private String myReturnType;
  public static final int GET_PROPERTY_MASK = 1;
  public static final int SET_PROPERTY_MASK = 2;
  public static final int CONSTRUCTOR_MASK = 4;
  private static final int DEPRECATED_MASK = 8;
  private static final int REFERENCES_ARGUMENTS_MASK = 16;

  protected JSFunctionStubBaseImpl(T function, final StubElement parent, final JSStubElementType elementType) {
    super(function,parent, elementType);
    myReturnType = function.getReturnTypeString();
  }

  protected int buildFlags(final T clazz) {
    final int val = clazz.isConstructor()
                    ? CONSTRUCTOR_MASK
                    : clazz.isGetProperty() ? GET_PROPERTY_MASK : clazz.isSetProperty() ? SET_PROPERTY_MASK : 0;
    return val | (clazz.isDeprecated() ? DEPRECATED_MASK:0) | (clazz.isReferencesArguments() ? REFERENCES_ARGUMENTS_MASK:0);
  }

  public JSFunctionStubBaseImpl(final StubInputStream dataStream, final StubElement parentStub,
                                final JSStubElementType elementType) throws IOException {
    super(dataStream, parentStub, elementType);
    myReturnType = readString(dataStream);
  }

  public JSFunctionStubBaseImpl(final String name, int flags, String qName, String returnType,final StubElement parentStub,
                                final JSStubElementType elementType) {
    super(name, flags, qName, parentStub, elementType);
    myReturnType = returnType;
  }

  public boolean isGetProperty() {
    return (myFlags & GET_PROPERTY_MASK) != 0;
  }

  public boolean isSetProperty() {
    return (myFlags & SET_PROPERTY_MASK) != 0;
  }

  public boolean isConstructor() {
    return (myFlags & CONSTRUCTOR_MASK) != 0;
  }

  public boolean isDeprecated() {
    return (myFlags & DEPRECATED_MASK) != 0;
  }

  public boolean isReferencesArguments() {
    return (myFlags & REFERENCES_ARGUMENTS_MASK) != 0;
  }

  protected boolean doIndexName(final String name, final String fqn) {
    final IStubElementType type = getStubType();
    if (type != JSElementTypes.FUNCTION_DECLARATION) return false;
    final IStubElementType stubType = getParentStub().getStubType();

    if (stubType instanceof JSPackageStatementElementType || stubType == null) {
      return true;
    }
    return false;
  }

  @Override
  protected boolean doIndexQualifiedName(final String name, final String fqn) {
    return doIndexName(name, fqn);
  }

  @Override
  public void serialize(final StubOutputStream dataStream) throws IOException {
    super.serialize(dataStream);
    writeString(myReturnType, dataStream);
  }

  public String getReturnTypeString() {
    return myReturnType;
  }
}