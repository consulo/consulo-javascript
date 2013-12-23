package com.intellij.lang.javascript.psi.impl;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLetExpression;
import com.intellij.psi.PsiElementVisitor;

/**
 * Created by IntelliJ IDEA.
 * User: maxim.mossienko
 * Date: Dec 14, 2005
 * Time: 6:40:04 PM
 * To change this template use File | Settings | File Templates.
 */
public class JSLetExpressionImpl extends JSExpressionImpl implements JSLetExpression
{
	public JSLetExpressionImpl(final ASTNode node)
	{
		super(node);
	}

	@Override
	public JSExpression getExpression()
	{
		final ASTNode expressionNode = getNode().findChildByType(JSElementTypes.EXPRESSIONS);
		return expressionNode != null ? (JSExpression) expressionNode.getPsi() : null;
	}

	@Override
	public void accept(@NotNull PsiElementVisitor visitor)
	{
		if(visitor instanceof JSElementVisitor)
		{
			((JSElementVisitor) visitor).visitJSLetExpression(this);
		}
		else
		{
			visitor.visitElement(this);
		}
	}
}