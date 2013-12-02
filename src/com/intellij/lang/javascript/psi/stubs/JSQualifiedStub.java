package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import com.intellij.psi.stubs.NamedStub;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 30, 2008
 *         Time: 9:30:52 PM
 */
public interface JSQualifiedStub<T extends JSQualifiedNamedElement> extends NamedStub<T>
{
	String getQualifiedName();
}
