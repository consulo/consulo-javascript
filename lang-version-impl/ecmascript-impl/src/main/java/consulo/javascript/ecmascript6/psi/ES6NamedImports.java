package consulo.javascript.ecmascript6.psi;

import com.intellij.lang.javascript.psi.JSElement;
import consulo.annotation.access.RequiredReadAction;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-14
 *
 * import { test } ...
 * .......^^^^^^^ named imports group
 */
public interface ES6NamedImports extends JSElement
{
	@Nonnull
	@RequiredReadAction
	ES6ImportedBinding[] getBindings();
}
