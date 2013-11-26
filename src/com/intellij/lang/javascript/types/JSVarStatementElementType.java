package com.intellij.lang.javascript.types;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.JSVarStatementStub;
import com.intellij.lang.javascript.psi.stubs.impl.JSVarStatementStubImpl;
import com.intellij.psi.PsiElement;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.util.io.PersistentStringEnumerator;

import java.io.IOException;

/**
 * @author Maxim.Mossienko
 *         Date: Jun 8, 2008
 *         Time: 1:50:59 PM
 */
public class JSVarStatementElementType extends JSStubElementType<JSVarStatementStub, JSVarStatement> {
  private static final JSStubGenerator<JSVarStatementStub,JSVarStatement> ourStubGenerator = new JSStubGenerator<JSVarStatementStub, JSVarStatement>() {
    public JSVarStatementStub newInstance(final StubInputStream dataStream,
                                          final StubElement parentStub,
                                          final JSStubElementType<JSVarStatementStub, JSVarStatement> type) throws IOException {
      return new JSVarStatementStubImpl(dataStream, parentStub, type);
    }

    public JSVarStatementStub newInstance(final JSVarStatement psi,
                                          final StubElement parentStub,
                                          final JSStubElementType<JSVarStatementStub, JSVarStatement> type) {
      return new JSVarStatementStubImpl(psi, parentStub, type);
    }
  };

  public JSVarStatementElementType() {
    super("VAR_STATEMENT", ourStubGenerator);
  }

  public boolean shouldCreateStub(ASTNode node) {
    final PsiElement element = node.getTreeParent().getPsi();
    final boolean b = element instanceof JSClass || element instanceof JSPackageStatement || element instanceof JSFile;
    return b;
  }
}
