package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.TokenSet;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import com.intellij.lang.javascript.psi.JSNamedElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import consulo.typescript.language.impl.psi.TypeScriptModuleUtil;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import consulo.typescript.language.psi.TypeScriptTypes;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

import java.util.List;

/**
 * A name an {@code import} declares: {@code import X from}, {@code import * as ns from}, {@code import { A as B } from}.
 * References resolve through it to what the module exports ({@link #resolveTargets()}).
 *
 * @since 2026-10-30
 */
public class TypeScriptImportBindingMixin extends TypeScriptElementImpl implements JSNamedElement {
    private static final TokenSet IDENTIFIERS = TokenSet.create(JSTokenTypes.IDENTIFIER);

    public TypeScriptImportBindingMixin(@Nonnull ASTNode node) {
        super(node);
    }

    /**
     * The local name: the last identifier ({@code B} of {@code A as B}).
     */
    @Nullable
    @Override
    @RequiredReadAction
    public PsiElement getNameIdentifier() {
        ASTNode[] identifiers = getNode().getChildren(IDENTIFIERS);
        return identifiers.length == 0 ? null : identifiers[identifiers.length - 1].getPsi();
    }

    @Override
    @RequiredReadAction
    public String getName() {
        PsiElement identifier = getNameIdentifier();
        return identifier != null ? identifier.getText() : null;
    }

    @Override
    @RequiredReadAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        TypeScriptResolveUtil.rename(getNameIdentifier(), name);
        return this;
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        return processor.execute(this, state);
    }

    /**
     * The module of the import declaration this binding belongs to.
     */
    @Nullable
    @RequiredReadAction
    public PsiFile resolveModule() {
        PsiElement declaration = getParent();
        while (declaration != null && declaration.getNode().getElementType() != TypeScriptTypes.IMPORT_DECLARATION) {
            declaration = declaration.getParent();
        }
        String specifier = declaration == null ? null : TypeScriptModuleUtil.getModuleSpecifier(declaration);
        return specifier == null ? null : TypeScriptModuleUtil.resolveModule(declaration, specifier);
    }

    /**
     * What the binding imports: the module itself for {@code * as ns}, else the exported declarations.
     */
    @RequiredReadAction
    public List<PsiElement> resolveTargets() {
        PsiFile module = resolveModule();
        if (module == null) {
            return List.of();
        }
        if (getNode().getElementType() == TypeScriptTypes.NAMESPACE_IMPORT) {
            return List.of(module);
        }
        if (getNode().getElementType() == TypeScriptTypes.DEFAULT_IMPORT_BINDING) {
            return TypeScriptModuleUtil.findExports(module, TypeScriptModuleUtil.DEFAULT_EXPORT);
        }
        ASTNode[] identifiers = getNode().getChildren(IDENTIFIERS);
        return identifiers.length == 0 ? List.of() : TypeScriptModuleUtil.findExports(module, identifiers[0].getText());
    }
}
