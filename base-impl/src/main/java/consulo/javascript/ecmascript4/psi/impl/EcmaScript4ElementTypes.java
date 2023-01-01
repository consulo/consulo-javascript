package consulo.javascript.ecmascript4.psi.impl;

import com.intellij.lang.javascript.psi.JSImportStatement;
import consulo.javascript.impl.language.psi.JSStubElementType;
import com.intellij.lang.javascript.psi.stubs.JSImportStatementStub;
import com.intellij.lang.javascript.types.JSImportStatementElementType;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public interface EcmaScript4ElementTypes
{
	JSStubElementType<JSImportStatementStub, JSImportStatement> IMPORT_STATEMENT = new JSImportStatementElementType();
}
