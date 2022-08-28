package consulo.javascript.impl.findUsages;

import consulo.annotation.component.ExtensionImpl;
import consulo.find.FindUsagesHandler;
import consulo.find.FindUsagesHandlerFactory;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import consulo.language.psi.PsiElement;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

/**
 * @author VISTALL
 * @since 2019-12-16
 */
@ExtensionImpl
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
