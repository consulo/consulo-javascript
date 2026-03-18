package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSCaseClause;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.typescript.language.impl.psi.TypeScriptElementImpl;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-03-18
 */
public class TypeScriptCaseClauseMixin extends TypeScriptElementImpl implements JSCaseClause {
    public TypeScriptCaseClauseMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSCaseClause(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Override
    public boolean isDefault() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child.getText().equals("default")) {
                return true;
            }
        }
        return false;
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getCaseExpression() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression expr) {
                return expr;
            }
        }
        return null;
    }

    @Override
    public JSStatement[] getStatements() {
        JSStatement[] result = PsiTreeUtil.getChildrenOfType(this, JSStatement.class);
        return result != null ? result : JSStatement.EMPTY;
    }
}
