package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptArgumentListMixin extends TypeScriptElementImpl implements JSArgumentList {
    public TypeScriptArgumentListMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSArgumentList(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Override
    public JSExpression[] getArguments() {
        JSExpression[] result = PsiTreeUtil.getChildrenOfType(this, JSExpression.class);
        return result != null ? result : JSExpression.EMPTY_ARRAY;
    }
}
