package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import consulo.language.psi.stub.IStubElementType;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript class declarations.
 * Extends StubBasedPsiElementBase to satisfy JSClass's StubBasedPsiElement contract.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptClassMixin extends StubBasedPsiElementBase<JSClassStub> implements JSClass {
    public TypeScriptClassMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public IStubElementType getElementType() {
        JSClassStub stub = getStub();
        if (stub != null) {
            return stub.getStubType();
        }
        return null;
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
        return null;
    }

    @Nullable
    @Override
    public JSReferenceList getImplementsList() {
        return null;
    }

    @Override
    public boolean isInterface() {
        return false;
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
        return getName();
    }

    // --- JSNamedElement / PsiNamedElement / PsiNameIdentifierOwner ---

    @RequiredReadAction
    @Override
    public String getName() {
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
}
