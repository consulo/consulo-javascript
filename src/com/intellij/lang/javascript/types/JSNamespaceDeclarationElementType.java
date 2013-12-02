package com.intellij.lang.javascript.types;

import java.io.IOException;

import com.intellij.lang.javascript.psi.JSNamespaceDeclaration;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSNamespaceDeclarationStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSNamespaceDeclarationStubImpl;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 6, 2008
 *         Time: 7:49:06 PM
 */
public class JSNamespaceDeclarationElementType extends JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration>
{
	private static final JSStubGenerator<JSNamespaceDeclarationStub, JSNamespaceDeclaration> ourStubGenerator = new
			JSStubGenerator<JSNamespaceDeclarationStub, JSNamespaceDeclaration>()
	{
		public JSNamespaceDeclarationStub newInstance(final StubInputStream dataStream, final StubElement parentStub,
				final JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> type) throws IOException
		{
			return new JSNamespaceDeclarationStubImpl(dataStream, parentStub, type);
		}

		public JSNamespaceDeclarationStub newInstance(final JSNamespaceDeclaration psi, final StubElement parentStub,
				final JSStubElementType<JSNamespaceDeclarationStub, JSNamespaceDeclaration> type)
		{
			return new JSNamespaceDeclarationStubImpl(psi, parentStub, type);
		}
	};

	public JSNamespaceDeclarationElementType()
	{
		super("NAMESPACE_DECLARATION", ourStubGenerator);
	}
}
