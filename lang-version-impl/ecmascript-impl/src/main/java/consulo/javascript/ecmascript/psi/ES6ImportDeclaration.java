package consulo.javascript.ecmascript.psi;

import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.psi.JavaScriptImportStatementBase;
import consulo.language.ast.ASTNode;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 *
 * import ...
 */
public interface ES6ImportDeclaration extends JavaScriptImportStatementBase {
    /**
     * {@code from '...'} of the declaration (also {@code import '...'}); null without a module specifier.
     */
    @Nullable
    @RequiredReadAction
    default ES6FromClause getFromClause() {
        for (ASTNode child = getNode().getFirstChildNode(); child != null; child = child.getTreeNext()) {
            if (JavaScriptTokenSets.STRING_LITERALS.contains(child.getElementType())) {
                String text = child.getText();
                return () -> text;
            }
        }
        return null;
    }
}
