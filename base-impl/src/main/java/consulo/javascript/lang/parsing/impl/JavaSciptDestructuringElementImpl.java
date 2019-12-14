package consulo.javascript.lang.parsing.impl;

import com.intellij.lang.ASTNode;
import com.intellij.lang.javascript.psi.JSDestructuringElement;
import com.intellij.lang.javascript.psi.JSDestructuringObject;
import com.intellij.lang.javascript.psi.impl.JSElementImpl;
import com.intellij.psi.PsiElementVisitor;
import consulo.annotation.access.RequiredReadAction;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-14
 */
public class JavaSciptDestructuringElementImpl extends JSElementImpl implements JSDestructuringElement
{
	public JavaSciptDestructuringElementImpl(ASTNode node)
	{
		super(node);
	}

	@Override
	public void accept(@Nonnull PsiElementVisitor visitor)
	{
		visitor.visitElement(this);
	}

	@RequiredReadAction
	@Nullable
	@Override
	public JSDestructuringObject getDestructuringObject()
	{
		return findChildByClass(JSDestructuringObject.class);
	}
}
