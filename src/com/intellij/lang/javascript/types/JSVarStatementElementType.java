package com.intellij.lang.javascript.types;

import java.io.IOException;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFile;
import com.intellij.lang.javascript.psi.JSPackageStatement;
import com.intellij.lang.javascript.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSVarStatementStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 8, 2008
 *         Time: 1:50:59 PM
 */
public class JSVarStatementElementType extends JSStubElementType<JSVarStatementStub, JSVarStatement>
{
	private static final JSStubGenerator<JSVarStatementStub, JSVarStatement> ourStubGenerator = new JSStubGenerator<JSVarStatementStub,
			JSVarStatement>()
	{
		public JSVarStatementStub newInstance(final StubInputStream dataStream, final StubElement parentStub, final JSStubElementType<JSVarStatementStub,
				JSVarStatement> type) throws IOException
		{
			return new JSVarStatementStubImpl(dataStream, parentStub, type);
		}

		public JSVarStatementStub newInstance(final JSVarStatement psi, final StubElement parentStub, final JSStubElementType<JSVarStatementStub,
				JSVarStatement> type)
		{
			return new JSVarStatementStubImpl(psi, parentStub, type);
		}
	};

	public JSVarStatementElementType()
	{
		super("VAR_STATEMENT", ourStubGenerator);
	}

	public boolean shouldCreateStub(ASTNode node)
	{
		final PsiElement element = node.getTreeParent().getPsi();
		final boolean b = element instanceof JSClass || element instanceof JSPackageStatement || element instanceof JSFile;
		return b;
	}
}
