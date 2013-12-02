package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSExpression;

/**
 * @author ven
 */
public abstract class JSExpressionImpl extends JSElementImpl implements JSExpression
{
	public JSExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	public JSExpression replace(JSExpression newExpr)
	{
		return JSChangeUtil.replaceExpression(this, newExpr);
	}
}
