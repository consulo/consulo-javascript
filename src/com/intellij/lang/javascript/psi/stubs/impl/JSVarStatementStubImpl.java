package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.DataInputStream;
import java.io.IOException;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.impl.JSVarStatementImpl;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 11:29:19 PM
 */
public class JSVarStatementStubImpl extends StubBase<JSVarStatement> implements JSVarStatementStub
{
	public JSVarStatementStubImpl(JSVarStatement clazz, final StubElement parent, final IStubElementType elementType)
	{
		super(parent, elementType);
	}

	public JSVarStatementStubImpl(final DataInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws IOException
	{
		super(parentStub, elementType);
	}

	public JSVarStatementStubImpl(final StubElement parentStub)
	{
		super(parentStub, JSElementTypes.VAR_STATEMENT);
	}

	@Override
	public JSVarStatement createPsi()
	{
		return new JSVarStatementImpl(this);
	}

	@Override
	public void index(final IndexSink sink)
	{
	}

	@Override
	public void serialize(final StubOutputStream dataStream) throws IOException
	{
	}
}