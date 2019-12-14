package consulo.javascript.lang.parsing.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSDestructuringShorthandedProperty;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import com.intellij.psi.PsiElementVisitor;

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
	public void accept(@Nonnull PsiElementVisitor visitor)
	{

	}
}
