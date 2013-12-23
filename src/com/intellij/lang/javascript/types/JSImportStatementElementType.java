package com.intellij.lang.javascript.types;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSImportStatement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSImportStatementStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSImportStatementStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:49:18 PM
 */
public class JSImportStatementElementType extends JSStubElementType<JSImportStatementStub, JSImportStatement>
{
	private static final JSStubGenerator<JSImportStatementStub, JSImportStatement> ourStubGenerator = new JSStubGenerator<JSImportStatementStub,
			JSImportStatement>()
	{
		@Override
		public JSImportStatementStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
				final JSStubElementType<JSImportStatementStub, JSImportStatement> type) throws IOException
		{
			return new JSImportStatementStubImpl(dataStream, parentStub, type);
		}

		@Override
		public JSImportStatementStub newInstance(final JSImportStatement psi, final StubElement parentStub, final JSStubElementType<JSImportStatementStub,
				JSImportStatement> type)
		{
			return new JSImportStatementStubImpl(psi, parentStub, type);
		}
	};

	public JSImportStatementElementType()
	{
		super("IMPORT_STATEMENT", ourStubGenerator);
	}
}
