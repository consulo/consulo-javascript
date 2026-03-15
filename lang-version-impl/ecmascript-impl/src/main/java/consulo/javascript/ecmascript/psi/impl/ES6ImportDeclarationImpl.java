package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.impl.JSStatementImpl;
import consulo.javascript.ecmascript.psi.ES6ImportDeclaration;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;


/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class ES6ImportDeclarationImpl extends JSStatementImpl implements ES6ImportDeclaration {
    public ES6ImportDeclarationImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }

    @Override
    public boolean processDeclarations(
        PsiScopeProcessor processor,
        ResolveState state,
        PsiElement lastParent,
        PsiElement place
    ) {
        for (PsiElement child : getChildren()) {
            if (!child.processDeclarations(processor, state, lastParent, place)) {
                return false;
            }
        }
        return true;
    }
}
