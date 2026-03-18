package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;
import org.jspecify.annotations.Nullable;

/**
 * Mixin for TypeScript parameters.
 * Extends TypeScriptVariableMixin since JSParameter extends JSVariable.
 *
 * @author VISTALL
 * @since 2026-03-17
 */
public class TypeScriptParameterMixin extends TypeScriptVariableMixin implements JSParameter {
    public TypeScriptParameterMixin(@Nonnull ASTNode node) {
        super(node);
    }

    @Override
    public void accept(@Nonnull PsiElementVisitor visitor) {
        if (visitor instanceof JSElementVisitor jsVisitor) {
            jsVisitor.visitJSParameter(this);
        }
        else {
            super.accept(visitor);
        }
    }

    // --- JSParameter ---

    @Override
    public JSFunction getDeclaringFunction() {
        return PsiTreeUtil.getParentOfType(this, JSFunction.class);
    }

    @RequiredReadAction
    @Override
    public boolean isRest() {
        return getRestElement() != null;
    }

    @Nullable
    @RequiredReadAction
    @Override
    public PsiElement getRestElement() {
        ASTNode dotDotDot = getNode().findChildByType(JSTokenTypes.DOT_DOT_DOT);
        return dotDotDot != null ? dotDotDot.getPsi() : null;
    }

    @Override
    public boolean isOptional() {
        ASTNode quest = getNode().findChildByType(JSTokenTypes.QUEST);
        return quest != null;
    }
}
