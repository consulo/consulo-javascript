package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;

import javax.annotation.Nonnull;

@ExtensionImpl
public class EmptyStatementBodyJSInspection extends JavaScriptInspection
{
	@Override
	@Nonnull
	public String getID()
	{
		return "StatementWithEmptyBodyJS";
	}

	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message("statement.with.empty.body.display.name");
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.CONFUSING_GROUP_NAME;
	}

	@Override
	public boolean isEnabledByDefault()
	{
		return true;
	}

	@RequiredReadAction
	@Override
	public String buildErrorString(Object state, Object... args)
	{
		if(args[0] instanceof JSIfStatement)
		{
			return InspectionJSBundle.message("statement.has.empty.branch.error.string");
		}
		else
		{
			return InspectionJSBundle.message("statement.has.empty.body.error.string");
		}
	}

	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new EmptyStatementBodyJSInspectionState();
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new EmptyStatementVisitor();
	}

	private class EmptyStatementVisitor extends BaseInspectionVisitor<EmptyStatementBodyJSInspectionState>
	{

		@Override
		public void visitJSDoWhileStatement(@Nonnull JSDoWhileStatement statement)
		{
			super.visitJSDoWhileStatement(statement);

			final JSStatement body = statement.getBody();
			if(body == null)
			{
				return;
			}
			if(!isEmpty(body))
			{
				return;
			}
			registerStatementError(statement, statement);
		}

		@Override
		public void visitJSWhileStatement(@Nonnull JSWhileStatement statement)
		{
			super.visitJSWhileStatement(statement);

			final JSStatement body = statement.getBody();
			if(body == null)
			{
				return;
			}
			if(!isEmpty(body))
			{
				return;
			}
			registerStatementError(statement, statement);
		}

		@Override
		public void visitJSForStatement(@Nonnull JSForStatement statement)
		{
			super.visitJSForStatement(statement);

			final JSStatement body = statement.getBody();
			if(body == null)
			{
				return;
			}
			if(!isEmpty(body))
			{
				return;
			}
			registerStatementError(statement, statement);
		}

		@Override
		public void visitJSForInStatement(@Nonnull JSForInStatement statement)
		{
			super.visitJSForInStatement(statement);

			final JSStatement body = statement.getBody();
			if(body == null)
			{
				return;
			}
			if(!isEmpty(body))
			{
				return;
			}
			registerStatementError(statement, statement);
		}

		@Override
		public void visitJSIfStatement(@Nonnull JSIfStatement statement)
		{
			super.visitJSIfStatement(statement);

			final JSStatement thenBranch = statement.getThen();
			if(thenBranch != null)
			{
				if(isEmpty(thenBranch))
				{
					registerStatementError(statement, statement);
					return;
				}
			}
			final JSStatement elseBranch = statement.getElse();

			if(elseBranch != null)
			{
				if(isEmpty(elseBranch))
				{
					registerStatementError(statement, statement);
				}
			}
		}

		private boolean isEmpty(JSElement body)
		{
			if(body instanceof JSEmptyStatement)
			{
				return true;
			}
			else if(myState.m_reportEmptyBlocks && body instanceof JSBlockStatement)
			{
				final JSBlockStatement block = (JSBlockStatement) body;
				final JSStatement[] statements = block.getStatements();
				return statements.length == 0;
			}
			return false;
		}
	}
}

