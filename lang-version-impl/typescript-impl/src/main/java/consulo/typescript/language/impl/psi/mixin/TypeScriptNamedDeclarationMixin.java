package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSNamedElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
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
 * TypeScript declarations without a JavaScript counterpart: {@code enum} (and its members), {@code namespace},
 * {@code type} aliases and type parameters.
 *
 * @since 2026-10-31
 */
public class TypeScriptNamedDeclarationMixin extends TypeScriptElementImpl implements JSNamedElement {
    public TypeScriptNamedDeclarationMixin(@Nonnull ASTNode node) {
        super(node);
    }

    /**
     * The name token: the IDENTIFIER of the declaration, the first one of {@code namespace A.B}, or the property name
     * (identifier or string) of an enum member.
     */
    @Nullable
    @Override
    @RequiredReadAction
    public PsiElement getNameIdentifier() {
        ASTNode node = getNode();
        ASTNode identifier = node.findChildByType(JSTokenTypes.IDENTIFIER);
        if (identifier == null) {
            ASTNode qualifiedName = node.findChildByType(TypeScriptTypes.QUALIFIED_NAME);
            identifier = qualifiedName != null ? qualifiedName.findChildByType(JSTokenTypes.IDENTIFIER) : null;
        }
        if (identifier == null && node.getElementType() == TypeScriptTypes.ENUM_MEMBER) {
            ASTNode first = node.getFirstChildNode();
            IElementType type = first == null ? null : first.getElementType();
            if (type == JSTokenTypes.STRING_LITERAL || type == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL) {
                identifier = first;
            }
        }
        return identifier != null ? identifier.getPsi() : null;
    }

    @Override
    @RequiredReadAction
    public String getName() {
        PsiElement identifier = getNameIdentifier();
        if (identifier == null) {
            return null;
        }
        String text = identifier.getText();
        IElementType type = identifier.getNode().getElementType();
        if ((type == JSTokenTypes.STRING_LITERAL || type == JSTokenTypes.SINGLE_QUOTE_STRING_LITERAL) && text.length() >= 2) {
            return text.substring(1, text.length() - 1);
        }
        return text;
    }

    @Override
    @RequiredReadAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        TypeScriptResolveUtil.rename(getNameIdentifier(), name);
        return this;
    }

    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        // seen from inside a generic type alias: its type parameters
        if (lastParent != null && !TypeScriptResolveUtil.processTypeParameters(this, processor, state)) {
            return false;
        }
        return getName() == null || processor.execute(this, state);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + getNode().getElementType() + ")";
    }
}
