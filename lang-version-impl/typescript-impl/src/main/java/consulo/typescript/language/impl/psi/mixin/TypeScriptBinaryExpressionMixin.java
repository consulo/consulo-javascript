package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.PsiComment;
import consulo.language.psi.PsiWhiteSpace;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript binary expressions and assignment expressions.
 * Implements JSBinaryExpression (JSAssignmentExpression extends it with no additional methods).
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptBinaryExpressionMixin extends TypeScriptExpressionMixin implements JSBinaryExpression {
    public TypeScriptBinaryExpressionMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSBinaryExpression(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSBinaryExpression ---

    @RequiredReadAction
    @Override
    public JSExpression getLOperand() {
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression expr) {
                return expr;
            }
        }
        return null;
    }

    @RequiredReadAction
    @Override
    public JSExpression getROperand() {
        JSExpression left = getLOperand();
        if (left == null) {
            return null;
        }
        for (PsiElement child = left.getNextSibling(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression expr) {
                return expr;
            }
        }
        return null;
    }

    @Nullable
    @RequiredReadAction
    @Override
    public PsiElement getOperationElement() {
        JSExpression left = getLOperand();
        if (left == null) {
            return null;
        }
        for (PsiElement child = left.getNextSibling(); child != null; child = child.getNextSibling()) {
            if (child instanceof JSExpression) {
                break;
            }
            if (!(child instanceof PsiWhiteSpace) && !(child instanceof PsiComment)) {
                return child;
            }
        }
        return null;
    }
}
