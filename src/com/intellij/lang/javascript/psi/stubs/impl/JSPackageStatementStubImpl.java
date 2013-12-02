package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSPackageStatementImpl;
import com.intellij.lang.javascript.psi.stubs.JSPackageStatementStub;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 8:00:52 PM
 */
public class JSPackageStatementStubImpl extends JSQualifiedObjectStubBase<JSPackageStatement> implements JSPackageStatementStub
{
	public JSPackageStatementStubImpl(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSPackageStatementStub,
			JSPackageStatement> type) throws IOException
	{
		super(dataStream, parentStub, type);
	}

	public JSPackageStatementStubImpl(final JSPackageStatement psi, final StubElement parentStub, final JSStubElementType<JSPackageStatementStub,
			JSPackageStatement> type)
	{
		super(psi, parentStub, type);
	}

	public JSPackageStatement createPsi()
	{
		return new JSPackageStatementImpl(this);
	}

	protected int buildFlags(final JSPackageStatement clazz)
	{
		return 0;
	}

	@Override
	protected boolean doIndexName(String name, String fqn)
	{
		return false;
	}

	@Override
	protected boolean doIndexQualifiedName(String name, String fqn)
	{
		return false;
	}
}