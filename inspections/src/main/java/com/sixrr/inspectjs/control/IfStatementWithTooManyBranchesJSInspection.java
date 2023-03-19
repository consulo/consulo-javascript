package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
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
public class IfStatementWithTooManyBranchesJSInspection extends JavaScriptInspection
{
	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message("if.statement.with.too.many.branches.display.name");
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
	}

	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new IfStatementWithTooManyBranchesJSInspectionState();
	}

	@RequiredReadAction
	@Override
	protected String buildErrorString(Object state, Object... args)
	{
		final JSIfStatement statement = (JSIfStatement) args[0];
		final int branches = calculateNumBranches(statement);
		return InspectionJSBundle.message("if.statement.with.too.many.branches.error.string", branches);
	}

	private static int calculateNumBranches(JSIfStatement statement)
	{
		final JSStatement branch = statement.getElse();
		if(branch == null)
		{
			return 1;
		}
		if(!(branch instanceof JSIfStatement))
		{
			return 2;
		}
		return 1 + calculateNumBranches((JSIfStatement) branch);
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new Visitor();
	}

	private class Visitor extends BaseInspectionVisitor<IfStatementWithTooManyBranchesJSInspectionState>
	{

		@Override
		public void visitJSIfStatement(@Nonnull JSIfStatement statement)
		{
			super.visitJSIfStatement(statement);
			final PsiElement parent = statement.getParent();
			if(parent instanceof JSIfStatement)
			{
				final JSIfStatement parentStatement = (JSIfStatement) parent;
				final JSStatement elseBranch = parentStatement.getElse();
				if(statement.equals(elseBranch))
				{
					return;
				}
			}
			final int branches = calculateNumBranches(statement);
			if(branches <= myState.m_limit)
			{
				return;
			}
			registerStatementError(statement, statement);
		}
	}
}
