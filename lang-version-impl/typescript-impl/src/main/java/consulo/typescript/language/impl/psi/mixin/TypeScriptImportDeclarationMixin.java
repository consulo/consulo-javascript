package consulo.typescript.language.impl.psi.mixin;

import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.typescript.language.impl.psi.TypeScriptModuleReference;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * {@code import ... from '...'}: declares its bindings.
 *
 * @since 2026-10-30
 */
public class TypeScriptImportDeclarationMixin extends TypeScriptStatementMixin {
    public TypeScriptImportDeclarationMixin(@Nonnull ASTNode node) {
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
        return processBindings(this, processor, state, place);
    }

    private static boolean processBindings(PsiElement element, PsiScopeProcessor processor, ResolveState state, PsiElement place) {
        for (PsiElement child = element.getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof TypeScriptImportBindingMixin binding) {
                if (!binding.processDeclarations(processor, state, null, place)) {
                    return false;
                }
            }
            else if (child.getFirstChild() != null && !processBindings(child, processor, state, place)) {
                return false;
            }
        }
        return true;
    }
}
