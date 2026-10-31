package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import consulo.language.psi.stub.IStubElementType;
import com.intellij.lang.javascript.JSTokenTypes;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import consulo.typescript.language.psi.TypeScriptTypes;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * A method, constructor, accessor of a class or a method signature of an interface.
 *
 * @since 2026-10-30
 */
public class TypeScriptMethodMixin<T extends JSFunctionStub> extends TypeScriptFunctionMixin<T> {
    public TypeScriptMethodMixin(@Nonnull ASTNode node) {
        super(node);
    }

    public TypeScriptMethodMixin(@Nonnull T stub, @Nonnull IStubElementType type) {
        super(stub, type);
    }

    // the stub first: the JavaScript resolve asks every member it meets, the declaring file (lib.dom.d.ts) isn't parsed

    @Override
    public boolean isGetProperty() {
        JSFunctionStub stub = getGreenStub();
        return stub != null ? stub.isGetProperty() : getNode().findChildByType(JSTokenTypes.GET_KEYWORD) != null;
    }

    @Override
    public boolean isSetProperty() {
        JSFunctionStub stub = getGreenStub();
        return stub != null ? stub.isSetProperty() : getNode().findChildByType(JSTokenTypes.SET_KEYWORD) != null;
    }

    @Override
    public boolean isConstructor() {
        return getElementType() == TypeScriptTypes.CONSTRUCTOR_DECLARATION;
    }

    /**
     * A member declares its parameters to its body; members aren't in scope without a qualifier
     * ({@code this.method()}), so it declares nothing to its siblings.
     */
    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        return lastParent == null || TypeScriptResolveUtil.processFunctionDeclarations(this, processor, state, lastParent);
    }
}
