package com.intellij.lang.javascript.search;

import org.jetbrains.annotations.Nullable;
import com.intellij.codeInsight.navigation.GotoTargetHandler;
import com.intellij.codeInsight.navigation.GotoTargetRendererProvider;
import com.intellij.ide.util.PsiElementListCellRenderer;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.navigation.NavigationItem;
import com.intellij.psi.PsiElement;

/**
 * @author Maxim.Mossienko
 *         Date: Apr 28, 2008
 *         Time: 8:14:14 PM
 */
public class JSGotoTargetRendererProvider implements GotoTargetRendererProvider
{
	static class JSClassListCellRenderer extends PsiElementListCellRenderer<JSNamedElement>
	{
		@Override
		public String getElementText(final JSNamedElement element)
		{
			return element.getName();
		}

		@Override
		protected String getContainerText(final JSNamedElement element, final String name)
		{
			final ItemPresentation presentation = ((NavigationItem) element).getPresentation();
			return presentation != null ? presentation.getLocationString() : null;
		}

		@Override
		protected int getIconFlags()
		{
			return 0;
		}
	}

	@Nullable
	@Override
	public PsiElementListCellRenderer getRenderer(PsiElement element, GotoTargetHandler.GotoData gotoData)
	{
		if(!(element instanceof JSNamedElement))
		{
			return null;
		}

		return new JSClassListCellRenderer();
	}
}
