package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import consulo.document.util.TextRange;
import consulo.language.ast.ASTNode;
import consulo.language.psi.*;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript reference expressions.
 * Implements JSReferenceExpression (which extends PsiPolyVariantReference).
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptReferenceExpressionMixin extends TypeScriptExpressionMixin implements JSReferenceExpression {
    public TypeScriptReferenceExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSReferenceExpression(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSReferenceExpression ---

    @Nullable
    @Override
    public JSExpression getQualifier() {
        return null;
    }

    @Nullable
    @Override
    public String getReferencedName() {
        PsiElement nameElement = getReferenceNameElement();
        return nameElement != null ? nameElement.getText() : null;
    }

    @Nullable
    @Override
    public PsiElement getReferenceNameElement() {
        ASTNode idNode = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
        return idNode != null ? idNode.getPsi() : null;
    }

    @Override
    public boolean shouldCheckReferences() {
        return false;
    }

    // --- PsiPolyVariantReference ---

    @Nonnull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        return ResolveResult.EMPTY_ARRAY;
    }

    // --- PsiReference ---

    @Nonnull
    @Override
    public PsiElement getElement() {
        return this;
    }

    @Nonnull
    @Override
    public TextRange getRangeInElement() {
        return TextRange.from(0, getTextLength());
    }

    @Nullable
    @Override
    public PsiElement resolve() {
        ResolveResult[] results = multiResolve(false);
        for (ResolveResult result : results) {
            if (result.isValidResult()) {
                return result.getElement();
            }
        }
        return null;
    }

    @Nonnull
    @Override
    public String getCanonicalText() {
        return getText();
    }

    @Override
    public PsiElement handleElementRename(@Nonnull String newElementName) throws IncorrectOperationException {
        return this;
    }

    @Override
    public PsiElement bindToElement(@Nonnull PsiElement element) throws IncorrectOperationException {
        return this;
    }

    @Override
    public boolean isReferenceTo(@Nonnull PsiElement element) {
        return false;
    }

    @Override
    public boolean isSoft() {
        return false;
    }

    @Override
    public PsiReference getReference() {
        return this;
    }
}
