package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSFunctionExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class NestedFunctionJSInspection extends JavaScriptInspection
{
	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message("nested.function.display.name");
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.CONFUSING_GROUP_NAME;
	}

	@RequiredReadAction
	@Override
	@Nullable
	protected String buildErrorString(Object state, Object... args)
	{
		final JSFunction function = (JSFunction) ((PsiElement) args[0]).getParent();
		if(functionHasIdentifier(function))
		{
			return InspectionJSBundle.message("nested.function.error.string");
		}
		return InspectionJSBundle.message("nested.anonymous.function.error.string");
	}


	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new NestedFunctionJSInspectionState();
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new Visitor();
	}

	private class Visitor extends BaseInspectionVisitor<NestedFunctionJSInspectionState>
	{
		@Override
		public void visitJSFunctionDeclaration(JSFunction function)
		{
			super.visitJSFunctionDeclaration(function);
			if(!myState.m_includeAnonymousFunctions && function.getName() == null)
			{
				return;
			}
			final JSFunction containingFunction = PsiTreeUtil.getParentOfType(function, JSFunction.class, true);
			if(containingFunction == null)
			{
				return;
			}
			registerFunctionError(function);
		}

		@Override
		public void visitJSFunctionExpression(final JSFunctionExpression node)
		{
			visitJSFunctionDeclaration(node.getFunction());
		}
	}
}
