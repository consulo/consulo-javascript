package com.intellij.lang.javascript.psi.impl;

import consulo.language.ast.ASTNode;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSClassExpression;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.psi.JavaScriptType;
import consulo.javascript.lang.psi.impl.JavaScriptClassType;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 11/12/2021
 */
public class JSClassExpressionImpl extends JSExpressionImpl implements JSClassExpression
{
	public JSClassExpressionImpl(ASTNode node)
	{
		super(node);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSExpression(this);
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public JSClass getClassElement()
	{
		return findNotNullChildByClass(JSClass.class);
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public JavaScriptType getType()
	{
		return new JavaScriptClassType(getClassElement());
	}
}
