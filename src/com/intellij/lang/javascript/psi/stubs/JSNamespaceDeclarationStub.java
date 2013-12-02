package com.intellij.lang.javascript.psi.stubs;

import com.intellij.lang.javascript.psi.JSNamespaceDeclaration;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:52:57 PM
 */
public interface JSNamespaceDeclarationStub extends JSStubElement<JSNamespaceDeclaration>, JSQualifiedStub<JSNamespaceDeclaration>
{
	String getInitialValueString();
}
