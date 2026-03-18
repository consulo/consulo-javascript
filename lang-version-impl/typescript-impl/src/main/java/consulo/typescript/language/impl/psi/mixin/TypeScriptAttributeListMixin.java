package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.stubs.JSAttributeListStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.impl.psi.stub.StubBasedPsiElementBase;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.stub.IStubElementType;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript attribute lists (modifiers).
 * Implements JSAttributeList to integrate with JavaScript PSI infrastructure.
 *
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptAttributeListMixin extends StubBasedPsiElementBase<JSAttributeListStub> implements JSAttributeList {
    public TypeScriptAttributeListMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public IStubElementType getElementType() {
        JSAttributeListStub stub = getStub();
        if (stub != null) {
            return stub.getStubType();
        }
        return null;
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSAttributeList(this);
        }
        else {
            visitor.visitElement(this);
        }
    }

    // --- JSAttributeList ---

    @Nullable
    @RequiredReadAction
    @Override
    public String getNamespace() {
        return null;
    }

    @Nullable
    @RequiredReadAction
    @Override
    public JSReferenceExpression getNamespaceElement() {
        return null;
    }

    @Override
    public JSAttribute[] getAttributes() {
        return JSAttribute.EMPTY_ARRAY;
    }

    @RequiredReadAction
    @Override
    public JSAttribute[] getAttributesByName(String name) {
        return JSAttribute.EMPTY_ARRAY;
    }

    @RequiredReadAction
    @Override
    public AccessType getAccessType() {
        return AccessType.PACKAGE_LOCAL;
    }

    @Nullable
    @RequiredReadAction
    @Override
    public PsiElement findAccessTypeElement() {
        return null;
    }

    @RequiredReadAction
    @Override
    public boolean hasModifier(ModifierType modifier) {
        return false;
    }
}
