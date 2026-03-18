package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.util.IncorrectOperationException;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript variable declarations.
 * Implements JSVariable to integrate with JavaScript PSI infrastructure.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptVariableMixin extends TypeScriptElementImpl implements JSVariable {
    public TypeScriptVariableMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSVariable(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSVariable ---

    @Override
    public boolean hasInitializer() {
        return getInitializer() != null;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getInitializer() {
        ASTNode eq = getNode().findChildByType(JSTokenTypes.EQ);
        if (eq != null) {
            for (PsiElement child = eq.getPsi().getNextSibling(); child != null; child = child.getNextSibling()) {
                if (child instanceof JSExpression expr) {
                    return expr;
                }
            }
        }
        return null;
    }

    @RequiredReadAction
    @Override
    public String getInitializerText() {
        JSExpression init = getInitializer();
        return init != null ? init.getText() : null;
    }

    @Override
    public void setInitializer(JSExpression expr) throws IncorrectOperationException {
        throw new IncorrectOperationException("Not yet implemented");
    }

    @RequiredReadAction
    @Override
    public JavaScriptType getType() {
        return null;
    }

    @Deprecated
    @Nullable
    @Override
    public String getTypeString() {
        return null;
    }

    @Nullable
    @RequiredReadAction
    @Override
    public JavaScriptTypeElement getTypeElement() {
        return null;
    }

    @Override
    public boolean isConst() {
        return false;
    }

    @Override
    public boolean isLocal() {
        return false;
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
}
