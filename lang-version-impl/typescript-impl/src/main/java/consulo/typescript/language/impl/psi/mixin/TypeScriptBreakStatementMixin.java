package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBreakStatement;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSLoopStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSSwitchStatement;
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
public class TypeScriptBreakStatementMixin extends TypeScriptStatementMixin implements JSBreakStatement {
    public TypeScriptBreakStatementMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSBreakStatement(this);
        }
        else {
            super.accept(visitor);
        }
    }

    @RequiredReadAction
    @Nullable
    @Override
    public String getLabel() {
        ASTNode idNode = getNode().findChildByType(JSTokenTypes.IDENTIFIER);
        return idNode != null ? idNode.getText() : null;
    }

    /**
     * The enclosing loop or switch, or the labeled statement of the label - not out of a function or a class static block
     */
    @RequiredReadAction
    @Nullable
    @Override
    public JSStatement getStatementToBreak() {
        String label = getLabel();
        for (PsiElement parent = getParent(); parent != null && !(parent instanceof JSFunction) && !(parent instanceof JSClassStaticBlock) && !(parent instanceof PsiFile);
             parent = parent.getParent()) {
            if (label == null ? parent instanceof JSLoopStatement || parent instanceof JSSwitchStatement : parent instanceof JSLabeledStatement labeled && label.equals(labeled.getLabel())) {
                return (JSStatement) parent;
            }
        }
        return null;
    }
}
