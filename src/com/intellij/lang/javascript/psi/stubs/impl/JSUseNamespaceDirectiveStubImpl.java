package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.JSUseNamespaceDirective;
import com.intellij.lang.javascript.psi.impl.JSUseNamespaceDirectiveImpl;
import com.intellij.lang.javascript.psi.stubs.JSUseNamespaceDirectiveStub;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 8:00:52 PM
 */
public class JSUseNamespaceDirectiveStubImpl extends StubBase<JSUseNamespaceDirective> implements JSUseNamespaceDirectiveStub
{
	private String myNamespaceToUse;

	public JSUseNamespaceDirectiveStubImpl(final StubInputStream dataStream, final StubElement parentStub,
			final JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> type) throws IOException
	{
		super(parentStub, type);
		final int idx = dataStream.readInt();
		myNamespaceToUse = idx != -1 ? dataStream.stringFromId(idx) : null;
	}

	public JSUseNamespaceDirectiveStubImpl(final JSUseNamespaceDirective psi, final StubElement parentStub,
			final JSStubElementType<JSUseNamespaceDirectiveStub, JSUseNamespaceDirective> type)
	{
		super(parentStub, type);

		myNamespaceToUse = psi.getNamespaceToBeUsed();
	}

	public JSUseNamespaceDirective createPsi()
	{
		return new JSUseNamespaceDirectiveImpl(this);
	}

	public void index(final IndexSink sink)
	{
	}

	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		dataStream.writeInt(myNamespaceToUse != null ? dataStream.getStringId(myNamespaceToUse) : -1);
	}

	public String getNamespaceToUse()
	{
		return myNamespaceToUse;
	}
}
