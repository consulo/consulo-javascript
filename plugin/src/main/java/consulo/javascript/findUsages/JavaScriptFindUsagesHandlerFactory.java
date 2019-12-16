package consulo.javascript.findUsages;

import com.intellij.find.findUsages.FindUsagesHandler;
import com.intellij.find.findUsages.FindUsagesHandlerFactory;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.psi.PsiElement;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-16
 */
public class JavaScriptFindUsagesHandlerFactory extends FindUsagesHandlerFactory
{
	@Override
	public boolean canFindUsages(@Nonnull PsiElement element)
	{
		return element instanceof JSDefinitionExpression;
	}

	@Nullable
	@Override
	public FindUsagesHandler createFindUsagesHandler(@Nonnull PsiElement element, boolean forHighlightUsages)
	{
		return new FindUsagesHandler(element)
		{

		};
	}
}
