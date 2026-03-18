package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.psi.JSComputedName;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptPropertyMixin extends TypeScriptElementImpl implements JSProperty {
    public TypeScriptPropertyMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSProperty(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSProperty ---

    @Nullable
    @Override
    public JSExpression getValue() {
        // Value is the expression after the colon
        boolean afterColon = false;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (afterColon && child instanceof JSExpression expr) {
                return expr;
            }
            if (child.getText().equals(":")) {
                afterColon = true;
            }
        }
        return null;
    }

    @Nullable
    @Override
    public PsiElement getColonElement() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child.getText().equals(":")) {
                return child;
            }
        }
        return null;
    }

    @Nullable
    @Override
    public JSComputedName getComputedName() {
        return PsiTreeUtil.getChildOfType(this, JSComputedName.class);
    }

    @Override
    public JavaScriptType getType() {
        return null;
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
        if (idNode != null) {
            return idNode.getPsi();
        }
        // Could also be a string literal property name
        ASTNode stringNode = getNode().findChildByType(JSTokenTypes.STRING_LITERAL);
        if (stringNode != null) {
            return stringNode.getPsi();
        }
        ASTNode numNode = getNode().findChildByType(JSTokenTypes.NUMERIC_LITERAL);
        return numNode != null ? numNode.getPsi() : null;
    }
}
