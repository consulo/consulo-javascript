package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.util.IncorrectOperationException;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptIfStatementMixin extends TypeScriptStatementMixin implements JSIfStatement {
    public TypeScriptIfStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSIfStatement(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getCondition() {
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
    public JSStatement getThen() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSStatement stmt) {
                return stmt;
            }
        }
        return null;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSStatement getElse() {
        boolean foundFirst = false;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSStatement) {
                if (foundFirst) {
                    return (JSStatement) child;
                }
                foundFirst = true;
            }
        }
        return null;
    }

    @Override
    public void setThen(JSStatement statement) throws IncorrectOperationException {
        throw new IncorrectOperationException("Not yet implemented");
    }

    @Override
    public void setElse(JSStatement statement) throws IncorrectOperationException {
        throw new IncorrectOperationException("Not yet implemented");
    }

    @Override
    public void setCondition(JSExpression expr) throws IncorrectOperationException {
        throw new IncorrectOperationException("Not yet implemented");
    }
}
