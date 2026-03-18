package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;

/**
 * Mixin for TypeScript block statements.
 * Extends TypeScriptStatementMixin for JSStatement methods, implements JSBlockStatement.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptBlockStatementMixin extends TypeScriptStatementMixin implements JSBlockStatement {
    public TypeScriptBlockStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSBlock(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSBlockStatement ---

    @Override
    public JSStatement[] getStatements() {
        JSStatement[] children = PsiTreeUtil.getChildrenOfType(this, JSStatement.class);
        return children != null ? children : JSStatement.EMPTY;
    }
}
