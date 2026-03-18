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
public class TypeScriptForStatementMixin extends TypeScriptStatementMixin implements JSForStatement {
    public TypeScriptForStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSForStatement(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSForStatement ---

    @RequiredReadAction
    @Nullable
    @Override
    public JSVarStatement getVarDeclaration() {
        return PsiTreeUtil.getChildOfType(this, JSVarStatement.class);
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getInitialization() {
        // The first expression inside '(' ... ';'
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
    public JSExpression getCondition() {
        // The second expression (after first ';')
        int semicolons = 0;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child.getText().equals(";")) {
                semicolons++;
            }
            if (semicolons == 1 && child instanceof JSExpression expr) {
                return expr;
            }
        }
        return null;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getUpdate() {
        // The third expression (after second ';')
        int semicolons = 0;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child.getText().equals(";")) {
                semicolons++;
            }
            if (semicolons == 2 && child instanceof JSExpression expr) {
                return expr;
            }
        }
        return null;
    }

    // --- JSLoopStatement ---

    @RequiredReadAction
    @Nullable
    @Override
    public JSStatement getBody() {
        // The last statement child is the body
        JSStatement last = null;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSStatement stmt) {
                last = stmt;
            }
        }
        return last;
    }
}
