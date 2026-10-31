package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSReferenceList;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.stub.IStubElementType;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import consulo.typescript.language.psi.TypeScriptTypes;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * {@code namespace Intl { ... }}: stubbed like a class (a global name with members, merged with the namespaces of the
 * same name of other files), so {@code Intl.DateTimeFormat} and {@code Reflect.ownKeys} resolve by the stubs. A
 * namespace is a value, not an interface.
 *
 * @since 2026-10-31
 */
public class TypeScriptNamespaceMixin<T extends JSClassStub> extends TypeScriptInterfaceMixin<T> {
    public TypeScriptNamespaceMixin(@Nonnull ASTNode node) {
        super(node);
    }

    public TypeScriptNamespaceMixin(@Nonnull JSClassStub stub, @Nonnull IStubElementType type) {
        super(stub, type);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + getElementType() + ")";
    }

    @Override
    public boolean isInterface() {
        return false;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public JSReferenceList getExtendsList() {
        return null;
    }

    /**
     * The first identifier of the name ({@code namespace A.B}); none for {@code declare module "x"}.
     */
    @RequiredReadAction
    @Nullable
    @Override
    public PsiElement getNameIdentifier() {
        ASTNode qualifiedName = getNode().findChildByType(TypeScriptTypes.QUALIFIED_NAME);
        ASTNode identifier = qualifiedName != null ? qualifiedName.findChildByType(JSTokenTypes.IDENTIFIER) : null;
        return identifier != null ? identifier.getPsi() : null;
    }

    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        if (TypeScriptResolveUtil.isMemberLookup(this, lastParent)) {
            return TypeScriptResolveUtil.processMembers(this, processor, state);
        }
        // seen from its body: the block declares the members
        return lastParent != null || getName() == null || processor.execute(this, state);
    }
}
