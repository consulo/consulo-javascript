package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSNamedElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import consulo.typescript.language.psi.TypeScriptTypes;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * A binding pattern: a name ({@code x}) or a destructuring pattern ({@code {a, b: c}}, {@code [x, ...rest]}). A name
 * inside a destructuring pattern is a declaration of its own.
 *
 * @since 2026-10-31
 */
public class TypeScriptBindingPatternMixin extends TypeScriptElementImpl implements JSNamedElement {
    public TypeScriptBindingPatternMixin(@Nonnull ASTNode node) {
        super(node);
    }

    /**
     * Whether the pattern is a single name.
     */
    public boolean isName() {
        ASTNode first = getNode().getFirstChildNode();
        return first != null && first == getNode().getLastChildNode() && first.getElementType() == JSTokenTypes.IDENTIFIER;
    }

    @Nullable
    @Override
    public PsiElement getNameIdentifier() {
        return isName() ? getNode().getFirstChildNode().getPsi() : null;
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

    /**
     * Declares the names of a destructuring pattern ({@code const {a, b: [c]} = ...} declares a and c).
     */
    @RequiredReadAction
    public boolean processNames(PsiScopeProcessor processor, ResolveState state) {
        if (isName()) {
            return processor.execute(this, state);
        }
        for (ASTNode child = getNode().getFirstChildNode(); child != null; child = child.getTreeNext()) {
            if (child.getElementType() == TypeScriptTypes.OBJECT_BINDING_PATTERN || child.getElementType() == TypeScriptTypes.ARRAY_BINDING_PATTERN) {
                for (ASTNode inner = child.getFirstChildNode(); inner != null; inner = inner.getTreeNext()) {
                    if (inner.getPsi() instanceof TypeScriptBindingPatternMixin pattern && !pattern.processNames(processor, state)) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        return true;
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + getNode().getElementType() + ")";
    }
}
