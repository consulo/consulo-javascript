package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.impl.JSExpressionImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.ecmascript.psi.ES6StringTemplateExpression;
import consulo.javascript.ecmascript.psi.ES6TaggedTemplateExpression;
import consulo.language.ast.ASTNode;
import consulo.language.psi.PsiElement;
import org.jspecify.annotations.Nullable;

/**
 * @author VISTALL
 * @since 2026-10-18
 */
public class ES6TaggedTemplateExpressionImpl extends JSExpressionImpl implements ES6TaggedTemplateExpression {
    public ES6TaggedTemplateExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public @Nullable JSExpression getTag() {
        PsiElement first = getFirstChild();
        return first instanceof JSExpression expression && !(first instanceof ES6StringTemplateExpression) ? expression : null;
    }

    @Override
    @RequiredReadAction
    public @Nullable ES6StringTemplateExpression getTemplateExpression() {
        return getLastChild() instanceof ES6StringTemplateExpression template ? template : null;
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSExpression(this);
    }
}
