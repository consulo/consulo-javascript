package consulo.javascript.ecmascript6.psi;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.psi.PsiNameIdentifierOwner;

/**
 * @author VISTALL
 * @since 2019-12-14
 *
 * import {import as importSpecifier} ...
 */
public interface ES6ImportSpecifier extends JSElement, PsiNameIdentifierOwner
{
}
