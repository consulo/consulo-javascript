package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSParameter;

/**
 * @author Maxim.Mossienko
 * Date: Mar 25, 2008
 * Time: 4:00:16 PM
 */
public interface JSParameterStub extends JSVariableStubBase<JSParameter> {
  boolean isRest();

  boolean isOptional();
}