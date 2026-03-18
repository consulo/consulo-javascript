package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.PsiComment;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptPrefixExpressionMixin extends TypeScriptExpressionMixin implements JSPrefixExpression {
    public TypeScriptPrefixExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSPrefixExpression(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @Nullable
    @Override
    public JSExpression getExpression() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression expr) {
                return expr;
            }
        }
        return null;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public IElementType getOperationSign() {
        PsiElement op = getOperatorElement();
        return op != null ? op.getNode().getElementType() : null;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public PsiElement getOperatorElement() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression) {
                break;
            }
            if (!(child instanceof PsiWhiteSpace) && !(child instanceof PsiComment)) {
                return child;
            }
        }
        return null;
    }
}
