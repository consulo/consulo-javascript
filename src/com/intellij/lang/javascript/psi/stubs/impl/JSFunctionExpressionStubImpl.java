package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSFunctionExpressionImpl;
import com.intellij.lang.javascript.psi.stubs.JSFunctionExpressionStub;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 26, 2008
 *         Time: 7:11:48 PM
 */
public class JSFunctionExpressionStubImpl extends JSFunctionStubBaseImpl<JSFunctionExpression> implements JSFunctionExpressionStub
{
	public JSFunctionExpressionStubImpl(JSFunctionExpression function, final StubElement parent, final JSStubElementType elementType)
	{
		super(function, parent, elementType);
	}

	public JSFunctionExpressionStubImpl(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType elementType) throws
			IOException
	{
		super(dataStream, parentStub, elementType);
	}

	public JSFunctionExpression createPsi()
	{
		return new JSFunctionExpressionImpl(this, JSElementTypes.FUNCTION_EXPRESSION);
	}
}