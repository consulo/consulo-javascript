package com.intellij.lang.javascript;

/**
 * @author Maxim.Mossienko
 */
public final class DialectOptionHolder {
  public final boolean isECMAL4Level;
  public final boolean isJavaScript1_6_OrBetter;
  public final boolean hasE4X;
  public final boolean isJavaScript1_7_OrBetter;
  public final boolean isJavaScript1_8_OrBetter;
  public final boolean isGwt;

  public DialectOptionHolder(boolean _ecma, boolean _gwt) {
    this(_ecma, _gwt, true);
  }

  public DialectOptionHolder(boolean _ecma, boolean _gwt, final boolean _e4x) {
    isECMAL4Level = _ecma;
    isGwt = _gwt;
    isJavaScript1_7_OrBetter = _e4x;
    isJavaScript1_8_OrBetter = _e4x;
    isJavaScript1_6_OrBetter = _e4x;
    hasE4X = _e4x;

    assert !isGwt || !isECMAL4Level;
  }
}
