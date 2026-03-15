package consulo.javascript.lang.parsing.impl;

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.psi.JSDestructuringElement;
import com.intellij.lang.javascript.psi.JSDestructuringObject;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;

import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class JavaSciptDestructuringElementImpl extends JSElementImpl implements JSDestructuringElement {
    public JavaSciptDestructuringElementImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }

    @RequiredReadAction
    @Nullable
    @Override
    public JSDestructuringObject getDestructuringObject() {
        return findChildByClass(JSDestructuringObject.class);
    }
}
