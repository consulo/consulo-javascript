package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSDoWhileStatement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSStatement;
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
public class TypeScriptDoWhileStatementMixin extends TypeScriptStatementMixin implements JSDoWhileStatement {
    public TypeScriptDoWhileStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSDoWhileStatement(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getCondition() {
        // In do-while, condition is after the 'while' keyword
        // do statement while ( expression ) ;
        // Find the last JSExpression child
        JSExpression last = null;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression expr) {
                last = expr;
            }
        }
        return last;
    }

    // --- JSLoopStatement ---

    @RequiredReadAction
    @Nullable
    @Override
    public JSStatement getBody() {
        // In do-while, body is the first statement child
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSStatement stmt) {
                return stmt;
            }
        }
        return null;
    }
}
