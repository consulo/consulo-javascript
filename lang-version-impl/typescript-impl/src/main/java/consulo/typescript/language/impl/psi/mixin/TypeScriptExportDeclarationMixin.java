package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import consulo.typescript.language.psi.TypeScriptTypes;
import consulo.typescript.language.impl.psi.TypeScriptModuleReference;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * {@code export <declaration>} / {@code declare <declaration>}: declares what the wrapped declaration declares.
 *
 * @since 2026-10-30
 */
public class TypeScriptExportDeclarationMixin extends TypeScriptElementImpl {
    public TypeScriptExportDeclarationMixin(@Nonnull ASTNode node) {
        super(node);
    }

    /**
     * The module specifier string ({@code from './x'}, {@code from 'pkg'}): Ctrl+click to the module file.
     */
    @Override
    @RequiredReadAction
    public PsiReference[] getReferences() {
        return TypeScriptModuleReference.create(this);
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
            if ((child instanceof JSElement || child instanceof TypeScriptElementImpl)
                && child.getNode().getElementType() != TypeScriptTypes.NAMED_EXPORTS
                && !child.processDeclarations(processor, state, null, place)) {
                return false;
            }
        }
        return true;
    }
}
