package consulo.javascript.lang.parsing.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSDestructuringShorthandedProperty;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import consulo.annotation.access.RequiredReadAction;

import javax.annotation.Nonnull;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class JavaScriptDestructuringShorthandedPropertyImpl extends JSElementImpl implements JSDestructuringShorthandedProperty
{
	public JavaScriptDestructuringShorthandedPropertyImpl(ASTNode node)
	{
		super(node);
	}

	@Override
	protected void accept(@Nonnull JSElementVisitor visitor)
	{
		visitor.visitJSElement(this);
	}

	@Nonnull
	@RequiredReadAction
	@Override
	public JSVariable getVarialbe()
	{
		return findNotNullChildByClass(JSVariable.class);
	}
}
