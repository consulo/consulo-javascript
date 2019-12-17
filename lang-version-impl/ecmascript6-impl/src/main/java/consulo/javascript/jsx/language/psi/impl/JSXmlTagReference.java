package consulo.javascript.jsx.language.psi.impl;

import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReferenceBase;
import consulo.annotation.access.RequiredReadAction;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-17
 */
public class JSXmlTagReference extends PsiReferenceBase<PsiElement>
{
	public JSXmlTagReference(@Nonnull PsiElement element)
	{
		super(element);
	}

	@RequiredReadAction
	@Nullable
	@Override
	public PsiElement resolve()
	{
		return null;
	}

	@RequiredReadAction
	@Nonnull
	@Override
	public Object[] getVariants()
	{
		return new Object[0];
	}
}
