package com.intellij.lang.javascript.types;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSFunctionExpressionStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSFunctionExpressionStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:50:34 PM
 */
public class JSFunctionExpressionElementType extends JSStubElementType<JSFunctionExpressionStub, JSFunctionExpression>
{
	private static final JSStubGenerator<JSFunctionExpressionStub, JSFunctionExpression> ourStubGenerator = new
			JSStubGenerator<JSFunctionExpressionStub, JSFunctionExpression>()
	{
		public JSFunctionExpressionStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
				final JSStubElementType<JSFunctionExpressionStub, JSFunctionExpression> elementType) throws IOException
		{
			return new JSFunctionExpressionStubImpl(dataStream, parentStub, elementType);
		}

		public JSFunctionExpressionStub newInstance(final JSFunctionExpression psi, final StubElement parentStub,
				final JSStubElementType<JSFunctionExpressionStub, JSFunctionExpression> elementType)
		{
			return new JSFunctionExpressionStubImpl(psi, parentStub, elementType);
		}
	};

	public JSFunctionExpressionElementType()
	{
		super("FUNCTION_EXPRESSION", ourStubGenerator);
	}
}