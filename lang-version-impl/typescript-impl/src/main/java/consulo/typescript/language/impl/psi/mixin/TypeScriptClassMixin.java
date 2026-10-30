package consulo.typescript.language.impl.psi.mixin;

import consulo.typescript.language.psi.TypeScriptTypes;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.stub.IStubElementType;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript class declarations.
 * Extends StubBasedPsiElementBase to satisfy JSClass's StubBasedPsiElement contract.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
// T only lets Grammar-Kit generate the stub constructor of the impl (it needs a generic super class)
public class TypeScriptClassMixin<T extends JSClassStub> extends StubBasedPsiElementBase<JSClassStub> implements JSClass {
    public TypeScriptClassMixin(@Nonnull ASTNode node) {
        super(node);
    }

    public TypeScriptClassMixin(@Nonnull JSClassStub stub, @Nonnull IStubElementType type) {
        super(stub, type);
    }


    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSClass(this);
        }
        else {
            visitor.visitElement(this);
        }
    }

    // --- JSClass ---

    @Nullable
    @Override
    public JSReferenceList getExtendsList() {
        return findChildByType(TypeScriptTypes.EXTENDS_CLAUSE) instanceof JSReferenceList list ? list : null;
    }

    @Nullable
    @Override
    public JSReferenceList getImplementsList() {
        return findChildByType(TypeScriptTypes.IMPLEMENTS_CLAUSE) instanceof JSReferenceList list ? list : null;
    }

    @Override
    public boolean isInterface() {
        return false;
    }

    @Override
    @RequiredReadAction
    public JSClass[] getSuperClasses() {
        return TypeScriptResolveUtil.getSuperClasses(this).toArray(JSClass.EMPTY_ARRAY);
    }

    @Override
    @RequiredReadAction
    public JSFunction[] getFunctions() {
        return TypeScriptResolveUtil.getClassMembers(this).stream()
            .filter(member -> member instanceof JSFunction)
            .toArray(JSFunction[]::new);
    }

    @Override
    @RequiredReadAction
    public JSVariable[] getFields() {
        return TypeScriptResolveUtil.getClassMembers(this).stream()
            .filter(member -> member instanceof JSVariable)
            .toArray(JSVariable[]::new);
    }

    @Nullable
    @Override
    @RequiredReadAction
    public JSFunction findFunctionByName(String name) {
        for (JSFunction function : getFunctions()) {
            if (name.equals(function.getName())) {
                return function;
            }
        }
        return null;
    }

    @Nullable
    @Override
    public JSFunction findFunctionByNameAndKind(String name, JSFunction.FunctionKind kind) {
        return null;
    }

    @Nullable
    @Override
    @RequiredReadAction
    public JSVariable findFieldByName(String name) {
        for (JSVariable field : getFields()) {
            if (name.equals(field.getName())) {
                return field;
            }
        }
        return null;
    }

    @Override
    public JSClass[] getSupers() {
        return JSClass.EMPTY_ARRAY;
    }

    @Override
    public JSClass[] getImplementedInterfaces() {
        return JSClass.EMPTY_ARRAY;
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
        return stub != null ? stub.getQualifiedName() : getName();
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
    public PsiElement setName(@Nonnull String name) {
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
    public String toString() {
        return getClass().getSimpleName() + ": " + getName();
    }

    /**
     * The class declares its own name, inside and outside of it. Unlike ActionScript, members aren't in scope without
     * {@code this.}: they are resolved through the qualifier.
     */
    @Override
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        // seen from inside: its type parameters too
        if (lastParent != null && !TypeScriptResolveUtil.processTypeParameters(this, processor, state)) {
            return false;
        }
        return getName() == null || processor.execute(this, state);
    }
}
