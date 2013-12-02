package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSVariable;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 4:00:16 PM
 */
public interface JSVariableStubBase<T extends JSVariable> extends JSQualifiedStub<T>, JSStubElement<T>
{
	String getTypeString();

	boolean isDeprecated();

	boolean isConst();

	String getInitializerText();

	boolean isLocal();
}