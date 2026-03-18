package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElementVisitor;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import jakarta.annotation.Nonnull;

/**
 * Base mixin for all TypeScript expression PSI elements.
 * Provides default implementations for JSExpression methods.
 */
public class TypeScriptExpressionMixin extends TypeScriptElementImpl implements JSExpression {
    public TypeScriptExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSExpression(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @Override
    public JSExpression replace(JSExpression other) {
        return (JSExpression) super.replace(other);
    }

    @RequiredReadAction
    @Override
    public JavaScriptType getType() {
        return null;
    }
}
