package com.intellij.lang.javascript.psi.stubs.impl;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSImportStatement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.impl.JSImportStatementImpl;
import com.intellij.lang.javascript.psi.stubs.JSImportStatementStub;
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
public class JSImportStatementStubImpl extends StubBase<JSImportStatement> implements JSImportStatementStub
{
	private String myImportText;

	public JSImportStatementStubImpl(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSImportStatementStub,
			JSImportStatement> type) throws IOException
	{
		super(parentStub, type);
		final int idx = dataStream.readInt();
		myImportText = idx != -1 ? dataStream.stringFromId(idx) : null;
	}

	public JSImportStatementStubImpl(final JSImportStatement psi, final StubElement parentStub, final JSStubElementType<JSImportStatementStub,
			JSImportStatement> type)
	{
		super(parentStub, type);

		myImportText = psi.getImportText();
	}

	public JSImportStatement createPsi()
	{
		return new JSImportStatementImpl(this);
	}

	public void index(final IndexSink sink)
	{
	}

	public void serialize(final StubOutputStream dataStream) throws IOException
	{
		dataStream.writeInt(myImportText != null ? dataStream.getStringId(myImportText) : -1);
	}

	public String getImportText()
	{
		return myImportText;
	}
}