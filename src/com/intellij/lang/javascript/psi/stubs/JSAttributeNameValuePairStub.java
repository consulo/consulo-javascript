package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSAttributeNameValuePair;
import com.intellij.psi.stubs.StubElement;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:52:57 PM
 */
public interface JSAttributeNameValuePairStub extends JSStubElement<JSAttributeNameValuePair>, StubElement<JSAttributeNameValuePair> {
  String getValue();
  String getName();
}