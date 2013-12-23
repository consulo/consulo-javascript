package com.intellij.lang.javascript.types;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSAttributeNameValuePair;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSAttributeNameValuePairStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSAttributeNameValuePairStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 8, 2008
 *         Time: 6:20:05 PM
 */
public class JSAttributeNameValuePairType extends JSStubElementType<JSAttributeNameValuePairStub, JSAttributeNameValuePair>
{
	private static final JSStubGenerator<JSAttributeNameValuePairStub, JSAttributeNameValuePair> ourStubGenerator = new
			JSStubGenerator<JSAttributeNameValuePairStub, JSAttributeNameValuePair>()
	{
		@Override
		public JSAttributeNameValuePairStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
				final JSStubElementType<JSAttributeNameValuePairStub, JSAttributeNameValuePair> type) throws IOException
		{
			return new JSAttributeNameValuePairStubImpl(dataStream, parentStub, type);
		}

		@Override
		public JSAttributeNameValuePairStub newInstance(final JSAttributeNameValuePair psi, final StubElement parentStub,
				final JSStubElementType<JSAttributeNameValuePairStub, JSAttributeNameValuePair> type)
		{
			return new JSAttributeNameValuePairStubImpl(psi, parentStub, type);
		}
	};

	public JSAttributeNameValuePairType()
	{
		super("ATTRIBUTE_NAME_VALUE_PAIR", ourStubGenerator);
	}
}
