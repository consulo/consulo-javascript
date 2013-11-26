package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSImportStatement;
import com.intellij.psi.stubs.StubElement;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:52:44 PM
 */
public interface JSImportStatementStub extends JSStubElement<JSImportStatement>, StubElement<JSImportStatement> {
  String getImportText();
}
