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
public class TypeScriptForInStatementMixin extends TypeScriptStatementMixin implements JSForInStatement {
    public TypeScriptForInStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSForInStatement(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSForInStatement ---

    @Nullable
    @Override
    public JSVarStatement getDeclarationStatement() {
        return PsiTreeUtil.getChildOfType(this, JSVarStatement.class);
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getVariableExpression() {
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
    public JSExpression getCollectionExpression() {
        // The expression after 'in' keyword
        JSExpression first = null;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression expr) {
                if (first != null) {
                    return expr;
                }
                first = expr;
            }
        }
        return null;
    }

    @Override
    public boolean isForEach() {
        return false;
    }

    // --- JSLoopStatement ---

    @RequiredReadAction
    @Nullable
    @Override
    public JSStatement getBody() {
        JSStatement last = null;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSStatement stmt) {
                last = stmt;
            }
        }
        return last;
    }
}
