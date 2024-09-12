package consulo.javascript.lang.parsing.impl;

import com.intellij.lang.javascript.psi.JSDestructuringObject;
import com.intellij.lang.javascript.psi.JSDestructuringShorthandedProperty;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.ast.ASTNode;

import jakarta.annotation.Nonnull;

import java.util.Arrays;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class JavaSciptDestructuringObjectImpl extends JSElementImpl implements JSDestructuringObject {
    public JavaSciptDestructuringObjectImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSElement(this);
    }

    @RequiredReadAction
    @Nonnull
    @Override
    public JSVariable[] getVariables() {
        JSDestructuringShorthandedProperty[] properties = getProperties();
        if (properties.length == 0) {
            return JSVariable.EMPTY_ARRAY;
        }
        return Arrays.stream(properties).map(JSDestructuringShorthandedProperty::getVarialbe).toArray(JSVariable[]::new);
    }

    @Override
    public JSDestructuringShorthandedProperty[] getProperties() {
        return findChildrenByClass(JSDestructuringShorthandedProperty.class);
    }
}
