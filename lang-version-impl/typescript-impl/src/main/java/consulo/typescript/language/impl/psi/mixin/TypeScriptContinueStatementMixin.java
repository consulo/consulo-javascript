package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSContinueStatement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSLoopStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.psi.JSClassStaticBlock;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.PsiFile;
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

    /**
     * The enclosing loop, or the labeled statement of the label - not out of a function or a class static block
     */
    @RequiredReadAction
    @Nullable
    @Override
    public JSStatement getStatementToContinue() {
        String label = getLabel();
        for (PsiElement parent = getParent(); parent != null && !(parent instanceof JSFunction) && !(parent instanceof JSClassStaticBlock) && !(parent instanceof PsiFile);
             parent = parent.getParent()) {
            if (label == null ? parent instanceof JSLoopStatement : parent instanceof JSLabeledStatement labeled && label.equals(labeled.getLabel()) && labeled.getStatement() instanceof JSLoopStatement) {
                return (JSStatement) parent;
            }
        }
        return null;
    }
}
