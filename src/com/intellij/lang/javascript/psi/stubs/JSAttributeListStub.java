package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.psi.stubs.StubElement;

/**
 * @author Maxim.Mossienko
 * Date: Mar 25, 2008
 * Time: 4:00:16 PM
 */
public interface JSAttributeListStub extends StubElement<JSAttributeList>, JSStubElement<JSAttributeList> {
  JSAttributeList.AccessType getAccessType();

  boolean hasModifier(final JSAttributeList.ModifierType modifier);

  String getNamespace();
}