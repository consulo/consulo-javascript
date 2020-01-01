package consulo.javascript.psi.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSSpreadExpression;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import com.intellij.psi.PsiElementVisitor;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.psi.JavaScriptType;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2020-01-01
 */
public class JavaScriptSpreadExpressionImpl extends JSElementImpl implements JSSpreadExpression
{
	public JavaScriptSpreadExpressionImpl(ASTNode node)
	{
		super(node);
	}

	@Override
	public JSExpression getInnerExpression()
	{
		return findNotNullChildByClass(JSExpression.class);
	}

	@Nonnull
	@Override
	public JSExpression replace(JSExpression other)
	{
		return this;
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public JavaScriptType getType()
	{
		return JavaScriptType.UNKNOWN;
	}

	@Override
	public void accept(@Nonnull PsiElementVisitor visitor)
	{
		visitor.visitElement(this);
	}
}
