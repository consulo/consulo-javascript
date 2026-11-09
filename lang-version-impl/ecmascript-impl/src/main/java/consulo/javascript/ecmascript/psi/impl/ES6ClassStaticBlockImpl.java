package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.javascript.psi.JSClassStaticBlock;
import consulo.language.ast.ASTNode;

/**
 * @author VISTALL
 * @since 2026-11-09
 */
public class ES6ClassStaticBlockImpl extends JSElementImpl implements JSClassStaticBlock {
    public ES6ClassStaticBlockImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }
}
