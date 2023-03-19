package com.sixrr.inspectjs.naming;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSParameter;
import com.intellij.lang.javascript.psi.JSParameterList;
import com.intellij.lang.javascript.psi.JSVariable;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.InspectionJSFix;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.fix.RenameFix;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ParameterNamingConventionJSInspection extends ConventionInspection
{
	private final RenameFix fix = new RenameFix();

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message("function.parameter.naming.convention.display.name");
	}

	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new ParameterNamingConventionJSInspectionState();
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.NAMING_CONVENTIONS_GROUP_NAME;
	}

	@Override
	protected InspectionJSFix buildFix(PsiElement location, Object state)
	{
		return fix;
	}

	@Override
	protected boolean buildQuickFixesOnlyForOnTheFlyErrors()
	{
		return true;
	}

	@RequiredReadAction
	@Override
	public String buildErrorString(Object state, Object... args)
	{
		ParameterNamingConventionJSInspectionState inspectionState = (ParameterNamingConventionJSInspectionState) state;

		final JSParameter parameter = (JSParameter) ((PsiElement) args[0]).getParent();
		assert parameter != null;
		final String parameterName = parameter.getName();
		if(parameterName.length() < inspectionState.m_minLength)
		{
			return InspectionJSBundle.message("parameter.name.is.too.short.error.string");
		}
		else if(parameterName.length() > inspectionState.m_maxLength)
		{
			return InspectionJSBundle.message("parameter.name.is.too.long.error.string");
		}
		return InspectionJSBundle.message("parameter.name.doesnt.match.regex.error.string", inspectionState.m_regex);
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new Visitor();
	}

	private class Visitor extends BaseInspectionVisitor<ParameterNamingConventionJSInspectionState>
	{
		@Override
		public void visitJSFunctionDeclaration(JSFunction function)
		{
			super.visitJSFunctionDeclaration(function);
			final JSParameterList parameterList = function.getParameterList();
			if(parameterList == null)
			{
				return;
			}
			final JSParameter[] parameters = parameterList.getParameters();
			for(JSVariable variable : parameters)
			{
				final String name = variable.getName();
				if(name == null)
				{
					continue;
				}
				if(isValid(name, myState))
				{
					continue;
				}
				registerVariableError(variable);
			}
		}
	}
}
