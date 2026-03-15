package consulo.javascript.lang.parsing.impl;

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.psi.JSDestructuringShorthandedProperty;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;


/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class JavaScriptDestructuringShorthandedPropertyImpl extends JSElementImpl implements JSDestructuringShorthandedProperty {
    public JavaScriptDestructuringShorthandedPropertyImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }

    @Override
    @RequiredReadAction
    public JSVariable getVariable() {
        return findNotNullChildByClass(JSVariable.class);
    }
}
