package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.impl.JSVariableImpl;
import com.intellij.lang.javascript.psi.stubs.JSVariableStub;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 11:29:19 PM
 */
public class JSVariableStubImpl extends JSVariableStubBaseImpl<JSVariable> implements JSVariableStub
{
	public JSVariableStubImpl(JSVariable clazz, final StubElement parent, final IStubElementType elementType)
	{
		super(clazz, parent, elementType);
	}

	public JSVariableStubImpl(final StubInputStream dataStream, final StubElement parentStub, final IStubElementType elementType) throws IOException
	{
		super(dataStream, parentStub, elementType);
	}

	public JSVariableStubImpl(final String name, int flags, String type, String initial, String qName, final StubElement parentStub)
	{
		super(name, flags, type, initial, qName, parentStub, JSElementTypes.VARIABLE);
	}

	public JSVariable createPsi()
	{
		return new JSVariableImpl(this);
	}
}