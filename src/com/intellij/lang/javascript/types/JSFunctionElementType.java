package com.intellij.lang.javascript.types;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSFunctionStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:50:34 PM
 */
public class JSFunctionElementType extends JSStubElementType<JSFunctionStub, JSFunction>
{
	private static final JSStubGenerator<JSFunctionStub, JSFunction> ourStubGenerator = new JSStubGenerator<JSFunctionStub, JSFunction>()
	{
		public JSFunctionStub newInstance(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSFunctionStub,
				JSFunction> elementType) throws IOException
		{
			return new JSFunctionStubImpl(dataStream, parentStub, elementType);
		}

		public JSFunctionStub newInstance(final JSFunction psi, final StubElement parentStub, final JSStubElementType<JSFunctionStub,
				JSFunction> elementType)
		{
			return new JSFunctionStubImpl(psi, parentStub, elementType);
		}
	};

	public JSFunctionElementType()
	{
		super("FUNCTION_DECLARATION", ourStubGenerator);
	}
}
