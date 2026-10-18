package consulo.javascript.ecmascript.psi.impl;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.impl.JSExpressionImpl;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.access.RequiredWriteAction;
import consulo.document.util.TextRange;
import consulo.javascript.ecmascript.psi.ES6StringTemplateExpression;
import consulo.javascript.language.psi.JavaScriptPrimitiveType;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.language.ast.ASTNode;
import consulo.language.psi.LiteralTextEscaper;
import consulo.language.psi.PsiLanguageInjectionHost;

/**
 * @author VISTALL
 * @since 2026-10-18
 */
public class ES6StringTemplateExpressionImpl extends JSExpressionImpl implements ES6StringTemplateExpression {
    public ES6StringTemplateExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    @RequiredReadAction
    public JSExpression[] getArguments() {
        return ES6StringTemplateUtil.getArguments(this);
    }

    @Override
    @RequiredReadAction
    public TextRange[] getStringRanges() {
        return ES6StringTemplateUtil.getStringRanges(this);
    }

    @Override
    @RequiredReadAction
    public TextRange[] getStringRangesWithEmpty() {
        return ES6StringTemplateUtil.getStringRangesWithEmpty(this);
    }

    @Override
    @RequiredReadAction
    public JavaScriptType getType() {
        return JavaScriptPrimitiveType.STRING;
    }

    @Override
    protected void accept(JSElementVisitor visitor) {
        visitor.visitJSExpression(this);
    }

    @Override
    public boolean isValidHost() {
        return true;
    }

    @Override
    @RequiredWriteAction
    public PsiLanguageInjectionHost updateText(String text) {
        return ES6StringTemplateUtil.updateText(this, text);
    }

    @Override
    public LiteralTextEscaper<? extends PsiLanguageInjectionHost> createLiteralTextEscaper() {
        return LiteralTextEscaper.createSimple(this);
    }
}
