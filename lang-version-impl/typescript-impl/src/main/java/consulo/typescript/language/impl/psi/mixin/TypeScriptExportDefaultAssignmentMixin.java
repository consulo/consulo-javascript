package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSElement;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * {@code export default <declaration | expression>}: an exported class / function is declared in its module too.
 *
 * @since 2026-10-31
 */
public class TypeScriptExportDefaultAssignmentMixin extends TypeScriptStatementMixin {
    public TypeScriptExportDefaultAssignmentMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        if (lastParent != null) {
            return true;
        }
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if ((child instanceof JSElement || child instanceof TypeScriptElementImpl) && !child.processDeclarations(processor, state, null, place)) {
                return false;
            }
        }
        return true;
    }
}
