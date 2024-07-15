package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class NestedFunctionCallJSInspection extends JavaScriptInspection
{
	@Override
	public boolean isEnabledByDefault()
	{
		return false;
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.STYLE_GROUP_NAME.get();
	}

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSLocalize.nestedFunctionCallDisplayName().get();
	}

	@RequiredReadAction
	@Override
	@Nonnull
	protected String buildErrorString(Object state, Object... args)
	{
		return InspectionJSLocalize.nestedFunctionCallProblemDescriptor().get();
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new NestedMethodCallVisitor();
	}

	private static class NestedMethodCallVisitor extends BaseInspectionVisitor
	{
		@Override
		public void visitJSCallExpression(@Nonnull JSCallExpression expression)
		{
			super.visitJSCallExpression(expression);
			JSExpression outerExpression = expression;
			while (outerExpression != null && outerExpression.getParent() instanceof JSExpression parentExpression)
			{
				outerExpression = parentExpression;
			}
			if (outerExpression == null)
			{
				return;
			}
			final PsiElement parent = outerExpression.getParent();
			if (!(parent instanceof JSArgumentList && parent.getParent() instanceof JSCallExpression)) {
				return;
			}
			registerFunctionCallError(expression);
		}
	}
}