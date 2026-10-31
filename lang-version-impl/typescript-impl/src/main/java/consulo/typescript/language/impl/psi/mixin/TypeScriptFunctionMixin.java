package consulo.typescript.language.impl.psi.mixin;

import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import com.intellij.lang.javascript.psi.stubs.JSFunctionStub;
import consulo.language.psi.stub.IStubElementType;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.psi.JSDeclaredTypeNameProvider;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript function declarations.
 * Implements JSFunction to integrate with JavaScript PSI infrastructure.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptFunctionMixin<T extends JSFunctionStub> extends StubBasedPsiElementBase<T> implements JSFunction, JSDeclaredTypeNameProvider {
    public TypeScriptFunctionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    public TypeScriptFunctionMixin(@Nonnull T stub, @Nonnull IStubElementType type) {
        super(stub, type);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSFunctionDeclaration(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSFunction ---

    @Nullable
    @RequiredReadAction
    @Override
    public JSParameterList getParameterList() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSParameterList paramList) {
                return paramList;
            }
        }
        return null;
    }

    @RequiredReadAction
    @Override
    public JSSourceElement[] getBody() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSBlockStatement block) {
                return new JSSourceElement[]{block};
            }
        }
        return JSSourceElement.EMPTY_ARRAY;
    }

    @Override
    public JavaScriptType getReturnType() {
        return JavaScriptType.UNKNOWN;
    }

    /**
     * The class name of the declared type (the return type): {@code HTMLElement | null} is {@code HTMLElement}.
     */
    @Nullable
    @Override
    @RequiredReadAction
    public String getDeclaredTypeName() {
        return TypeScriptResolveUtil.getTypeName(getReturnTypeString());
    }

    @Override
    @RequiredReadAction
    public String getReturnTypeString() {
        JSFunctionStub stub = getGreenStub();
        return stub != null ? stub.getReturnTypeString() : TypeScriptResolveUtil.getTypeAnnotationText(this);
    }

    @Nullable
    @Override
    public JavaScriptTypeElement getReturnTypeElement() {
        return null;
    }

    @Override
    public boolean isDeprecated() {
        return false;
    }

    // --- JSQualifiedNamedElement ---

    @RequiredReadAction
    @Override
    public String getQualifiedName() {
        var stub = getGreenStub();
        return stub != null ? stub.getQualifiedName() : TypeScriptResolveUtil.qualifyByNamespaces(this, getName());
    }

    // --- JSNamedElement / PsiNamedElement / PsiNameIdentifierOwner ---

    @RequiredReadAction
    @Override
    public String getName() {
        var stub = getGreenStub();
        if (stub != null) {
            return stub.getName();
        }
        PsiElement id = getNameIdentifier();
        return id != null ? id.getText() : null;
    }

    @Override
    @RequiredReadAction
    public PsiElement setName(@Nonnull String name) throws IncorrectOperationException {
        TypeScriptResolveUtil.rename(getNameIdentifier(), name);
        return this;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public PsiElement getNameIdentifier() {
        ASTNode idNode = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
        return idNode != null ? idNode.getPsi() : null;
    }

    // --- JSAttributeListOwner ---

    @Nullable
    @Override
    public JSAttributeList getAttributeList() {
        return null;
    }

    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        return TypeScriptResolveUtil.processFunctionDeclarations(this, processor, state, lastParent);
    }

    @Override
    public String toString() {
        return getClass().getSimpleName() + "(" + getElementType() + ")";
    }
}
