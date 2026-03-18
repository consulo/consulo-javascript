package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript call expressions and new expressions.
 * JSNewExpression extends JSCallExpression with no additional methods.
 *
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptCallExpressionMixin extends TypeScriptExpressionMixin implements JSCallExpression {
    public TypeScriptCallExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSCallExpression(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getMethodExpression() {
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
    public JSArgumentList getArgumentList() {
        return PsiTreeUtil.getChildOfType(this, JSArgumentList.class);
    }
}
