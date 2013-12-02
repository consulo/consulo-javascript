package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSIncludeDirective;
import com.intellij.psi.stubs.StubElement;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:52:57 PM
 */
public interface JSIncludeDirectiveStub extends JSStubElement<JSIncludeDirective>, StubElement<JSIncludeDirective>
{
	String getIncludeText();
}