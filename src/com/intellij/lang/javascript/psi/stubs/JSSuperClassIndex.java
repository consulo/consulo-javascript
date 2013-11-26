package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.types.JSFileElementType;
import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;

public class JSSuperClassIndex extends StringStubIndexExtension<JSClass> {
  public static final StubIndexKey<String, JSClass> KEY = StubIndexKey.createIndexKey("JS.class.super");
  private static final int VERSION = 1;

  public StubIndexKey<String, JSClass> getKey() {
    return KEY;
  }

  @Override
  public int getVersion() {
    return super.getVersion() + VERSION + JSFileElementType.VERSION;
  }
}