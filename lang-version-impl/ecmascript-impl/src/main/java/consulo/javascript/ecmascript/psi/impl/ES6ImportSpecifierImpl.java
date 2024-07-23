package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.javascript.ecmascript.psi.ES6ImportSpecifier;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2020-08-26
 */
public class ES6ImportSpecifierImpl extends JSElementImpl implements ES6ImportSpecifier {
    public ES6ImportSpecifierImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }

    @RequiredReadAction
    @Nullable
    @Override
    public PsiElement getNameIdentifier() {
        return findChildByType(JSTokenTypes.IDENTIFIER);
    }

    @RequiredWriteAction
    @Override
    public PsiElement setName(@Nonnull String s) throws IncorrectOperationException {
        return null;
    }
}
