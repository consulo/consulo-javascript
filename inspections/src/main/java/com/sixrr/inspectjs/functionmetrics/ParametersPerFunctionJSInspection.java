package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ParametersPerFunctionJSInspection extends JavaScriptInspection
{
	@Override
	@Nonnull
	public String getID()
	{
		return "OverlyComplexFunctionJS";
	}

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message("function.with.too.many.parameters.display.name");
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME;
	}

	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new ParametersPerFunctionJSInspectionState();
	}

	@RequiredReadAction
	@Override
	public String buildErrorString(Object state, Object... args)
	{
		final JSFunction function = (JSFunction) ((PsiElement) args[0]).getParent();
		assert function != null;
		final JSParameterList parameterList = function.getParameterList();
		final JSParameter[] parameters = parameterList.getParameters();
		final int numParameters = parameters.length;
		if(functionHasIdentifier(function))
		{
			return InspectionJSBundle.message("function.has.too.many.parameters.error.string", numParameters);
		}
		else
		{
			return InspectionJSBundle.message("anonymous.function.has.too.many.parameters.error.string", numParameters);
		}
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new Visitor();
	}

	private class Visitor extends BaseInspectionVisitor<ParametersPerFunctionJSInspectionState>
	{

		@Override
		public void visitJSFunctionDeclaration(@Nonnull JSFunction function)
		{
			final JSParameterList parameterList = function.getParameterList();
			if(parameterList == null)
			{
				return;
			}
			final JSParameter[] parameters = parameterList.getParameters();
			if(parameters == null)
			{
				return;
			}
			final int numParameters = parameters.length;
			if(numParameters <= myState.getLimit())
			{
				return;
			}
			registerFunctionError(function);
		}
	}
}

