package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSArrayLiteralExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptArrayLiteralExpressionMixin extends TypeScriptExpressionMixin implements JSArrayLiteralExpression {
    public TypeScriptArrayLiteralExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSArrayLiteralExpression(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Override
    public JSExpression[] getExpressions() {
        JSExpression[] result = PsiTreeUtil.getChildrenOfType(this, JSExpression.class);
        return result != null ? result : JSExpression.EMPTY_ARRAY;
    }
}
