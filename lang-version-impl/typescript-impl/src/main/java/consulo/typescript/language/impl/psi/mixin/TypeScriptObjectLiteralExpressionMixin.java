package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSObjectLiteralExpression;
import com.intellij.lang.javascript.psi.JSProperty;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptObjectLiteralExpressionMixin extends TypeScriptExpressionMixin implements JSObjectLiteralExpression {
    public TypeScriptObjectLiteralExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSObjectLiteralExpression(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Override
    public JSProperty[] getProperties() {
        JSProperty[] result = PsiTreeUtil.getChildrenOfType(this, JSProperty.class);
        return result != null ? result : new JSProperty[0];
    }
}
