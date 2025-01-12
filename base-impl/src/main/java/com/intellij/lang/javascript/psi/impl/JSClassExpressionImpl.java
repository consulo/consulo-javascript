package com.intellij.lang.javascript.psi.impl;

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSClassExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.lang.psi.impl.JavaScriptClassType;

import jakarta.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2021-12-11
 */
public class JSClassExpressionImpl extends JSExpressionImpl implements JSClassExpression {
    public JSClassExpressionImpl(ASTNode node) {
        super(node);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSExpression(this);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public JSClass getClassElement() {
        return findNotNullChildByClass(JSClass.class);
    }

    @Nonnull
    @Override
    @RequiredReadAction
    public JavaScriptType getType() {
        return new JavaScriptClassType(getClassElement());
    }
}
