package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSFunctionImpl;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 7:11:48 PM
 */
public class JSFunctionStubImpl extends JSFunctionStubBaseImpl<JSFunction> implements JSFunctionStub
{
	public JSFunctionStubImpl(JSFunction function, final StubElement parent, final JSStubElementType elementType)
	{
		super(function, parent, elementType);
	}

	public JSFunctionStubImpl(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType elementType) throws IOException
	{
		super(dataStream, parentStub, elementType);
	}

	public JSFunctionStubImpl(final String name, int flags, String qName, String returnType, final StubElement parentStub)
	{
		super(name, flags, qName, returnType, parentStub, JSElementTypes.FUNCTION_DECLARATION);
	}

	@Override
	public JSFunction createPsi()
	{
		return new JSFunctionImpl(this, JSElementTypes.FUNCTION_DECLARATION);
	}
}