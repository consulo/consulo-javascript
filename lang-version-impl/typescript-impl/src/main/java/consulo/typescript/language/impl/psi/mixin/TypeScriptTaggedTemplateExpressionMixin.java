package consulo.typescript.language.impl.psi.mixin;

import com.intellij.lang.javascript.psi.JSExpression;
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
public class TypeScriptTaggedTemplateExpressionMixin extends TypeScriptExpressionMixin implements ES6TaggedTemplateExpression {
    public TypeScriptTaggedTemplateExpressionMixin(ASTNode node) {
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
}
