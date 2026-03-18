package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.language.psi.JavaScriptTypeElement;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for arrow_function and function_expression rules.
 * Extends TypeScriptExpressionMixin (since these are expressions)
 * and implements JSFunction (since JSFunctionExpression/JavaScriptLambdaExpression extend JSFunction).
 *
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptFunctionExpressionMixin extends TypeScriptExpressionMixin implements JSFunction {
    public TypeScriptFunctionExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            // At runtime, 'this' will be the generated impl which implements JSFunctionExpression
            jsVisitor.visitJSExpression(this);
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
        // Arrow functions may have an expression body instead of a block
        // e.g. (x) => x + 1
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSSourceElement sourceElem && !(child instanceof JSParameterList)) {
                return new JSSourceElement[]{sourceElem};
            }
        }
        return JSSourceElement.EMPTY_ARRAY;
    }

    @Override
    public JavaScriptType getReturnType() {
        return null;
    }

    @Override
    public String getReturnTypeString() {
        return null;
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
