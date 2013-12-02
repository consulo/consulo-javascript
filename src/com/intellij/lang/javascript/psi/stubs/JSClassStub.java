package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSClass;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 4:00:16 PM
 */
public interface JSClassStub extends JSStubElement<JSClass>, JSQualifiedStub<JSClass>
{
	boolean isInterface();

	boolean isDeprecated();
}
