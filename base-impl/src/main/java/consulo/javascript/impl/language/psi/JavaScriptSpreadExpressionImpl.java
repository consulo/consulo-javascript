package consulo.javascript.impl.language.psi;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSSpreadExpression;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.language.ast.ASTNode;


/**
 * @author VISTALL
 * @since 2020-01-01
 */
public class JavaScriptSpreadExpressionImpl extends JSElementImpl implements JSSpreadExpression {
    public JavaScriptSpreadExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    public JSExpression getInnerExpression() {
        return findNotNullChildByClass(JSExpression.class);
    }

    @Override
    public JSExpression replace(JSExpression other) {
        return this;
    }

    @RequiredReadAction
    @Override
    public JavaScriptType getType() {
        return JavaScriptType.UNKNOWN;
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }
}
