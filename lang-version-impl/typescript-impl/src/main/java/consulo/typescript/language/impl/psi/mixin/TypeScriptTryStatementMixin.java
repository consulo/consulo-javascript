package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptTryStatementMixin extends TypeScriptStatementMixin implements JSTryStatement {
    public TypeScriptTryStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSTryStatement(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSStatement getStatement() {
        // The first block statement (try body)
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSStatement stmt && !(child instanceof JSCatchBlock)) {
                return stmt;
            }
        }
        return null;
    }

    @RequiredReadAction
    @Override
    public JSCatchBlock[] getAllCatchBlocks() {
        JSCatchBlock[] result = PsiTreeUtil.getChildrenOfType(this, JSCatchBlock.class);
        return result != null ? result : JSCatchBlock.EMPTY_ARRAY;
    }

    @Deprecated
    @RequiredReadAction
    @Nullable
    @Override
    public JSCatchBlock getCatchBlock() {
        JSCatchBlock[] blocks = getAllCatchBlocks();
        return blocks.length > 0 ? blocks[0] : null;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSStatement getFinallyStatement() {
        // The statement after 'finally' keyword
        boolean foundFinally = false;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (foundFinally && child instanceof JSStatement stmt) {
                return stmt;
            }
            if (child.getText().equals("finally")) {
                foundFinally = true;
            }
        }
        return null;
    }
}
