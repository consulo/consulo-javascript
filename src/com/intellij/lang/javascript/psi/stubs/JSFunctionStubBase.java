package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSFunction;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 4:00:16 PM
 */
public interface JSFunctionStubBase<T extends JSFunction> extends JSStubElement<T>, JSQualifiedStub<T>
{
	String getReturnTypeString();

	boolean isGetProperty();

	boolean isSetProperty();

	boolean isConstructor();

	boolean isDeprecated();

	boolean isReferencesArguments();
}