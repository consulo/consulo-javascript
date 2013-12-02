package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.psi.stubs.StubElement;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:52:51 PM
 */
public interface JSReferenceListStub extends JSStubElement<JSReferenceList>, StubElement<JSReferenceList>
{
	String[] getReferenceTexts();
}
