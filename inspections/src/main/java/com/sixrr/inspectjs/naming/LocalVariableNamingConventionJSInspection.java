package com.sixrr.inspectjs.naming;

import com.intellij.lang.javascript.psi.JSVarStatement;
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

import jakarta.annotation.Nonnull;

@ExtensionImpl
public class LocalVariableNamingConventionJSInspection extends ConventionInspection
{
	private final RenameFix fix = new RenameFix();

	@Override
	public boolean isEnabledByDefault()
	{
		return false;
	}

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message("local.variable.naming.convention.display.name");
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
		LocalVariableNamingConventionJSInspectionState inspectionState = (LocalVariableNamingConventionJSInspectionState) state;

		final JSVariable variable = (JSVariable) ((PsiElement) args[0]).getParent();
		assert variable != null;
		final String variableName = variable.getName();
		if(variableName.length() < inspectionState.m_minLength)
		{
			return InspectionJSBundle.message("variable.name.is.too.short.error.string");
		}
		else if(variableName.length() > inspectionState.m_maxLength)
		{
			return InspectionJSBundle.message("variable.name.is.too.long.error.string");
		}
		return InspectionJSBundle.message("variable.name.doesnt.match.regex.error.string", inspectionState.m_regex);
	}

	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new LocalVariableNamingConventionJSInspectionState();
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new Visitor();
	}

	private class Visitor extends BaseInspectionVisitor<LocalVariableNamingConventionJSInspectionState>
	{
		@Override
		public void visitJSVarStatement(JSVarStatement jsVarStatement)
		{
			super.visitJSVarStatement(jsVarStatement);
			final JSVariable[] variables = jsVarStatement.getVariables();
			for(JSVariable variable : variables)
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
