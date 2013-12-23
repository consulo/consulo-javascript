package com.intellij.lang.javascript.types;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSAttribute;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSAttributeStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSAttributeStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Mar 25, 2008
 *         Time: 10:30:20 PM
 */
public class JSAttributeElementType extends JSStubElementType<JSAttributeStub, JSAttribute>
{
	public JSAttributeElementType()
	{
		super("ATTRIBUTE", new JSStubGenerator<JSAttributeStub, JSAttribute>()
		{
			@Override
			public JSAttributeStub newInstance(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSAttributeStub,
					JSAttribute> elementType) throws IOException
			{
				return new JSAttributeStubImpl(dataStream, parentStub, elementType);
			}

			@Override
			public JSAttributeStub newInstance(final JSAttribute psi, final StubElement parentStub, final JSStubElementType<JSAttributeStub,
					JSAttribute> elementType)
			{
				return new JSAttributeStubImpl(psi, parentStub, elementType);
			}
		});
	}
}
