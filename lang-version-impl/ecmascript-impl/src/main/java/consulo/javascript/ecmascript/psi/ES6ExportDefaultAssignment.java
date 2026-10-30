package consulo.javascript.ecmascript.psi;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.PsiElement;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public interface ES6ExportDefaultAssignment extends JSStatement {
    /**
     * The exported expression of {@code export default <expression>}; null when a declaration is exported
     * ({@code export default class A {}}).
     */
    @Nullable
    @RequiredReadAction
    default JSExpression getExpression() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression expression) {
                return expression;
            }
        }
        return null;
    }
}
