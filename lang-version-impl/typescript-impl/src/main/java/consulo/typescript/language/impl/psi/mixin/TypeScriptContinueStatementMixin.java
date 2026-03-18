package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSContinueStatement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElementVisitor;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptContinueStatementMixin extends TypeScriptStatementMixin implements JSContinueStatement {
    public TypeScriptContinueStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSContinueStatement(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @Nullable
    @Override
    public String getLabel() {
        ASTNode idNode = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
        return idNode != null ? idNode.getText() : null;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSStatement getStatementToContinue() {
        // Would require walking up the tree to find enclosing loop statement
        return null;
    }
}
