package consulo.javascript.ecmascript6.psi;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.psi.PsiNameIdentifierOwner;
import consulo.annotation.access.RequiredReadAction;

import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 * <p>
 * import ImportedBinding ....
 * <p>
 * asterisk import also imported binding
 * <p>
 * import * as util ...
 */
public interface ES6ImportedBinding extends JSElement, PsiNameIdentifierOwner
{
	@Nullable
	@RequiredReadAction
	ES6ImportSpecifier getImportSpecifier();
}
