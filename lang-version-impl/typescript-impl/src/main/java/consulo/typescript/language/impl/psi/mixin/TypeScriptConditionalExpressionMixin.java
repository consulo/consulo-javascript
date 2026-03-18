package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptConditionalExpressionMixin extends TypeScriptExpressionMixin implements JSConditionalExpression {
    public TypeScriptConditionalExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSConditionalExpression(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getCondition() {
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
    public JSExpression getThen() {
        int count = 0;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression) {
                count++;
                if (count == 2) {
                    return (JSExpression) child;
                }
            }
        }
        return null;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getElse() {
        int count = 0;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression) {
                count++;
                if (count == 3) {
                    return (JSExpression) child;
                }
            }
        }
        return null;
    }
}
