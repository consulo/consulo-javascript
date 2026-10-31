package consulo.typescript.language.impl.psi.mixin;

import consulo.typescript.language.impl.stub.TypeScriptStubElementTypes;
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
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
import consulo.language.psi.stub.IStubElementType;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript interface declarations.
 * Implements JSClass with isInterface() returning true.
 *
 * @author VISTALL
 * @since 2026-03-18
 */
// T only lets Grammar-Kit generate the stub constructor of the impl (it needs a generic super class)
public class TypeScriptInterfaceMixin<T extends JSClassStub> extends StubBasedPsiElementBase<JSClassStub> implements JSClass {
    public TypeScriptInterfaceMixin(@Nonnull ASTNode node) {
        super(node);
    }

    public TypeScriptInterfaceMixin(@Nonnull JSClassStub stub, @Nonnull IStubElementType type) {
        super(stub, type);
    }


    @Override
    public String toString() {
        return getClass().getSimpleName() + ":" + getElementType();
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
    @RequiredReadAction
    public JSReferenceList getExtendsList() {
        // the stub of the list when the interface has one: inherited members resolve without the AST
        return getStubOrPsiChild(TypeScriptStubElementTypes.EXTENDS_LIST);
    }

    @Nullable
    @Override
    public JSReferenceList getImplementsList() {
        return null;
    }

    @Override
    public boolean isInterface() {
        return true;
    }

    @Override
    public JSClass[] getSuperClasses() {
        return JSClass.EMPTY_ARRAY;
    }

    @Override
    public JSFunction[] getFunctions() {
        return JSFunction.EMPTY_ARRAY;
    }

    @Override
    public JSVariable[] getFields() {
        return JSVariable.EMPTY_ARRAY;
    }

    @Nullable
    @Override
    public JSFunction findFunctionByName(String name) {
        return null;
    }

    @Nullable
    @Override
    public JSFunction findFunctionByNameAndKind(String name, JSFunction.FunctionKind kind) {
        return null;
    }

    @Nullable
    @Override
    public JSVariable findFieldByName(String name) {
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
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        if (TypeScriptResolveUtil.isMemberLookup(this, lastParent)) {
            return TypeScriptResolveUtil.processMembers(this, processor, state);
        }
        if (lastParent != null && !TypeScriptResolveUtil.processTypeParameters(this, processor, state)) {
            return false;
        }
        return processor.execute(this, state);
    }
}
