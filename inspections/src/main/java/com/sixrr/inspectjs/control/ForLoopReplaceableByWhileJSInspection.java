package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSVarStatement;
import com.sixrr.inspectjs.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.editor.inspection.ProblemDescriptor;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.project.Project;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ForLoopReplaceableByWhileJSInspection extends JavaScriptInspection
{
	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message("for.loop.replaceable.by.while.display.name");
	}

	@Override
	@Nonnull
	public String getID()
	{
		return "ForLoopReplaceableByWhile";
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
	}

	@RequiredReadAction
	@Override
	@Nonnull
	protected String buildErrorString(Object state, Object... args)
	{
		return InspectionJSBundle.message(
				"for.loop.replaceable.by.while.problem.descriptor");
	}

	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new ForLoopReplaceableByWhileJSInspectionState();
	}

	@Override
	public InspectionJSFix buildFix(PsiElement location, Object state)
	{
		return new ReplaceForByWhileFix();
	}

	private static class ReplaceForByWhileFix extends InspectionJSFix
	{

		@Override
		@Nonnull
		public String getName()
		{
			return InspectionJSBundle.message(
					"for.loop.replaceable.by.while.replace.quickfix");
		}

		@Override
		public void doFix(Project project, ProblemDescriptor descriptor) throws IncorrectOperationException
		{
			final PsiElement forKeywordElement = descriptor.getPsiElement();
			final JSForStatement forStatement =
					(JSForStatement) forKeywordElement.getParent();
			assert forStatement != null;
			final JSExpression condition = forStatement.getCondition();
			final JSStatement body = forStatement.getBody();
			final String bodyText;
			if(body == null)
			{
				bodyText = "";
			}
			else
			{
				bodyText = body.getText();
			}
			@NonNls final String whileStatement;
			if(condition == null)
			{
				whileStatement = "while(true)" + bodyText;
			}
			else
			{
				whileStatement = "while(" + condition.getText() + ')' +
						bodyText;
			}
			replaceStatement(forStatement, whileStatement);
		}
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new ForLoopReplaceableByWhileVisitor();
	}

	private class ForLoopReplaceableByWhileVisitor extends BaseInspectionVisitor<ForLoopReplaceableByWhileJSInspectionState>
	{

		@Override
		public void visitJSForStatement(@Nonnull JSForStatement statement)
		{
			super.visitJSForStatement(statement);
			final JSVarStatement varStatement = statement.getVarDeclaration();
			if(varStatement != null)
			{
				return;
			}
			final JSExpression initialization = statement.getInitialization();
			if(initialization != null)
			{
				return;
			}
			final JSExpression update = statement.getUpdate();
			if(update != null)
			{
				return;
			}
			if(myState.m_ignoreLoopsWithoutConditions)
			{
				final JSExpression condition = statement.getCondition();
				if(condition == null)
				{
					return;
				}
				final String conditionText = condition.getText();
				if("true".equals(conditionText))
				{
					return;
				}
			}
			registerStatementError(statement);
		}
	}
}