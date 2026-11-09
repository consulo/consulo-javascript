package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.PsiUtilCore;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.typescript.language.impl.psi.TypeScriptResolveUtil;
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

    /**
     * for (x in o), for (a.b of c) - the target expression before the keyword; null for a declaration
     */
    @RequiredReadAction
    @Nullable
    @Override
    public JSExpression getVariableExpression() {
        for (PsiElement child = getFirstChild(); child != null && !isInOrOf(child); child = child.getNextSibling()) {
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
        boolean afterKeyword = false;
        for (PsiElement child = getFirstChild(); child != null; child = child.getNextSibling()) {
            if (isInOrOf(child)) {
                afterKeyword = true;
            }
            else if (afterKeyword && child instanceof JSExpression expr) {
                return expr;
            }
        }
        return null;
    }

    @RequiredReadAction
    private static boolean isInOrOf(PsiElement element) {
        IElementType type = PsiUtilCore.getElementType(element);
        return type == JSTokenTypes.IN_KEYWORD || type == JSTokenTypes.OF_KEYWORD;
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

    // JavaScript's JSForStatementImpl / JSForInStatementImpl: the loop variables are seen from inside the loop
    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState state,
        @Nullable PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        return lastParent == null || TypeScriptResolveUtil.processChildVariables(this, processor, state, lastParent);
    }
}
