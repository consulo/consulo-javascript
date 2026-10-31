package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSQualifiedNamedElement;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.psi.JSTypeOnlyDeclaration;
import consulo.language.ast.ASTNode;
import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.stub.IStubElementType;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import consulo.typescript.language.impl.stub.TypeScriptTypeAliasStub;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * {@code type Record<K, V> = ...}: a type only, stubbed when global (lib.es5.d.ts) or of a namespace, so type
 * references resolve to it by the index.
 *
 * @since 2026-10-31
 */
// T only lets Grammar-Kit generate the stub constructor of the impl (it needs a generic super class)
public class TypeScriptTypeAliasMixin<T extends TypeScriptTypeAliasStub> extends StubBasedPsiElementBase<TypeScriptTypeAliasStub>
    implements JSQualifiedNamedElement, JSTypeOnlyDeclaration {
    public TypeScriptTypeAliasMixin(@Nonnull ASTNode node) {
        super(node);
    }

    public TypeScriptTypeAliasMixin(@Nonnull TypeScriptTypeAliasStub stub, @Nonnull IStubElementType type) {
        super(stub, type);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSElement(this);
        }
        else {
            visitor.visitElement(this);
        }
    }

    @Nullable
    @Override
    @RequiredReadAction
    public String getName() {
        TypeScriptTypeAliasStub stub = getGreenStub();
        if (stub != null) {
            return stub.getName();
        }
        PsiElement identifier = getNameIdentifier();
        return identifier != null ? identifier.getText() : null;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public String getQualifiedName() {
        TypeScriptTypeAliasStub stub = getGreenStub();
        return stub != null ? stub.getQualifiedName() : TypeScriptResolveUtil.qualifyByNamespaces(this, getName());
    }

    @Nullable
    @Override
    @RequiredReadAction
    public PsiElement getNameIdentifier() {
        ASTNode identifier = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
        return identifier != null ? identifier.getPsi() : null;
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
        // seen from inside: its type parameters
        if (lastParent != null && !TypeScriptResolveUtil.processTypeParameters(this, processor, state)) {
            return false;
        }
        return getName() == null || processor.execute(this, state);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + getElementType() + ")";
    }
}
