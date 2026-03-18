package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSPostfixExpression;
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
public class TypeScriptPostfixExpressionMixin extends TypeScriptExpressionMixin implements JSPostfixExpression {
    public TypeScriptPostfixExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSPostfixExpression(this);
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
    @Override
    public IElementType getOperationSign() {
        // In postfix expression, the operator is after the expression
        PsiElement lastSignificant = null;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (!(child instanceof PsiWhiteSpace) && !(child instanceof PsiComment)) {
                lastSignificant = child;
            }
        }
        if (lastSignificant != null && !(lastSignificant instanceof JSExpression)) {
            return lastSignificant.getNode().getElementType();
        }
        return null;
    }
}
