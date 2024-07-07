package com.sixrr.inspectjs.dataflow;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import consulo.language.psi.util.PsiTreeUtil;

import jakarta.annotation.Nonnull;

@ExtensionImpl
public class UnnecessaryLocalVariableJSInspection extends JavaScriptInspection
{
	@Override
	@Nonnull
	public String getDisplayName()
	{
		return InspectionJSBundle.message(
				"redundant.local.variable.display.name");
	}

	@Override
	@Nonnull
	public String getGroupDisplayName()
	{
		return JSGroupNames.DATA_FLOW_ISSUES;
	}

	@Nonnull
	@Override
	public InspectionToolState<?> createStateProvider()
	{
		return new UnnecessaryLocalVariableJSInspectionState();
	}

	@Override
	public boolean isEnabledByDefault()
	{
		return true;
	}

	@RequiredReadAction
	@Override
	@Nonnull
	public String buildErrorString(Object state, Object... args)
	{
		return InspectionJSBundle.message(
				"unnecessary.local.variable.problem.descriptor");
	}

	@Override
	public BaseInspectionVisitor buildVisitor()
	{
		return new UnnecessaryLocalVariableVisitor();
	}

	@Override
	protected boolean buildQuickFixesOnlyForOnTheFlyErrors()
	{
		return true;
	}

	private class UnnecessaryLocalVariableVisitor extends BaseInspectionVisitor<UnnecessaryLocalVariableJSInspectionState>
	{
		@Override
		public void visitJSVarStatement(@Nonnull JSVarStatement varStatement)
		{
			super.visitJSVarStatement(varStatement);
			final JSVariable[] variables = varStatement.getVariables();
			for(JSVariable variable : variables)
			{

				if(isCopyVariable(variable))
				{
					registerVariableError(variable);
				}
				else if(!myState.m_ignoreImmediatelyReturnedVariables && isImmediatelyReturned(variable))
				{
					registerVariableError(variable);
				}
				else if(!myState.m_ignoreImmediatelyReturnedVariables && isImmediatelyThrown(variable))
				{
					registerVariableError(variable);
				}
				else if(isImmediatelyAssigned(variable))
				{
					registerVariableError(variable);
				}
				else if(isImmediatelyAssignedAsDeclaration(variable))
				{
					registerVariableError(variable);
				}
			}
		}

		private boolean isCopyVariable(JSVariable variable)
		{
			final JSExpression initializer = variable.getInitializer();
			if(initializer == null)
			{
				return false;
			}
			if(!(initializer instanceof JSReferenceExpression))
			{
				return false;
			}
			final JSReferenceExpression reference =
					(JSReferenceExpression) initializer;
			final PsiElement referent = reference.resolve();
			if(referent == null)
			{
				return false;
			}
			if(!(referent instanceof JSVariable))
			{
				return false;
			}
			final JSBlockStatement containingScope =
					PsiTreeUtil.getParentOfType(variable, JSBlockStatement.class);
			if(containingScope == null || isClassMember(referent))
			{
				return false;
			}
			final VariableAssignedVisitor visitor =
					new VariableAssignedVisitor(variable);
			containingScope.accept(visitor);
			if(visitor.isAssigned())
			{
				return false;
			}
			final JSVariable initialization = (JSVariable) referent;
			final VariableAssignedVisitor visitor2 =
					new VariableAssignedVisitor(initialization);
			containingScope.accept(visitor2);
			if(visitor2.isAssigned())
			{
				return false;
			}

			return !variableIsUsedInInnerFunction(containingScope, variable);
		}

		private boolean isClassMember(final PsiElement referent)
		{
			final PsiElement grandParent = referent.getParent().getParent();
			return grandParent instanceof JSClass || (grandParent instanceof JSFile && grandParent.getContext() != null);
		}

		private boolean isImmediatelyReturned(JSVariable variable)
		{
			final JSBlockStatement containingScope =
					PsiTreeUtil.getParentOfType(variable, JSBlockStatement.class);
			if(containingScope == null)
			{
				return false;
			}
			final JSVarStatement declarationStatement =
					PsiTreeUtil.getParentOfType(variable,
							JSVarStatement.class);
			if(declarationStatement == null)
			{
				return false;
			}
			JSStatement nextStatement = null;
			final JSStatement[] statements = containingScope.getStatements();
			for(int i = 0; i < statements.length - 1; i++)
			{
				if(statements[i].equals(declarationStatement))
				{
					nextStatement = statements[i + 1];
				}
			}
			if(nextStatement == null)
			{
				return false;
			}
			if(!(nextStatement instanceof JSReturnStatement))
			{
				return false;
			}
			final JSReturnStatement returnStatement =
					(JSReturnStatement) nextStatement;
			final JSExpression returnValue = returnStatement.getExpression();
			if(returnValue == null)
			{
				return false;
			}
			if(!(returnValue instanceof JSReferenceExpression))
			{
				return false;
			}
			final PsiElement referent = ((PsiReference) returnValue).resolve();
			return !(referent == null || !referent.equals(variable));
		}

		private boolean isImmediatelyThrown(JSVariable variable)
		{
			final JSBlockStatement containingScope =
					PsiTreeUtil.getParentOfType(variable, JSBlockStatement.class);
			if(containingScope == null)
			{
				return false;
			}
			final JSVarStatement declarationStatement =
					PsiTreeUtil.getParentOfType(variable,
							JSVarStatement.class);
			if(declarationStatement == null)
			{
				return false;
			}
			JSStatement nextStatement = null;
			final JSStatement[] statements = containingScope.getStatements();
			for(int i = 0; i < statements.length - 1; i++)
			{
				if(statements[i].equals(declarationStatement))
				{
					nextStatement = statements[i + 1];
				}
			}
			if(nextStatement == null)
			{
				return false;
			}
			if(!(nextStatement instanceof JSThrowStatement))
			{
				return false;
			}
			final JSThrowStatement throwStatement =
					(JSThrowStatement) nextStatement;
			final JSExpression returnValue = throwStatement.getExpression();
			if(returnValue == null)
			{
				return false;
			}
			if(!(returnValue instanceof JSReferenceExpression))
			{
				return false;
			}
			final PsiElement referent = ((PsiReference) returnValue).resolve();
			return !(referent == null || !referent.equals(variable));
		}

		private boolean isImmediatelyAssigned(JSVariable variable)
		{
			final JSBlockStatement containingScope =
					PsiTreeUtil.getParentOfType(variable, JSBlockStatement.class);
			if(containingScope == null)
			{
				return false;
			}
			final JSVarStatement declarationStatement =
					PsiTreeUtil.getParentOfType(variable,
							JSVarStatement.class);
			if(declarationStatement == null)
			{
				return false;
			}
			JSStatement nextStatement = null;
			int followingStatementNumber = 0;
			final JSStatement[] statements = containingScope.getStatements();
			for(int i = 0; i < statements.length - 1; i++)
			{
				if(statements[i].equals(declarationStatement))
				{
					nextStatement = statements[i + 1];
					followingStatementNumber = i + 2;
				}
			}
			if(nextStatement == null)
			{
				return false;
			}
			if(!(nextStatement instanceof JSExpressionStatement))
			{
				return false;
			}
			final JSExpressionStatement expressionStatement =
					(JSExpressionStatement) nextStatement;
			final JSExpression expression =
					expressionStatement.getExpression();
			if(!(expression instanceof JSAssignmentExpression))
			{
				return false;
			}
			final JSAssignmentExpression assignmentExpression =
					(JSAssignmentExpression) expression;
			final JSExpression rhs = assignmentExpression.getROperand();
			if(rhs == null)
			{
				return false;
			}
			if(!(rhs instanceof JSReferenceExpression))
			{
				return false;
			}
			final JSReferenceExpression reference =
					(JSReferenceExpression) rhs;
			final PsiElement referent = reference.resolve();
			if(referent == null || !referent.equals(variable))
			{
				return false;
			}
			final JSExpression lhs = assignmentExpression.getLOperand();
			if(VariableAccessUtils.variableIsUsed(variable, lhs))
			{
				return false;
			}
			for(int i = followingStatementNumber; i < statements.length; i++)
			{
				if(VariableAccessUtils.variableIsUsed(variable,
						statements[i]))
				{
					return false;
				}
			}
			return true;
		}

		private boolean isImmediatelyAssignedAsDeclaration(
				JSVariable variable)
		{
			final JSBlockStatement containingScope =
					PsiTreeUtil.getParentOfType(variable, JSBlockStatement.class);
			if(containingScope == null)
			{
				return false;
			}
			final JSVarStatement declarationStatement =
					PsiTreeUtil.getParentOfType(variable,
							JSVarStatement.class);
			if(declarationStatement == null)
			{
				return false;
			}
			JSStatement nextStatement = null;
			int followingStatementNumber = 0;
			final JSStatement[] statements = containingScope.getStatements();
			for(int i = 0; i < statements.length - 1; i++)
			{
				if(statements[i].equals(declarationStatement))
				{
					nextStatement = statements[i + 1];
					followingStatementNumber = i + 2;
				}
			}
			if(nextStatement == null)
			{
				return false;
			}
			if(!(nextStatement instanceof JSVarStatement))
			{
				return false;
			}
			final JSVarStatement declaration =
					(JSVarStatement) nextStatement;
			final JSVariable[] declarations = declaration.getVariables();
			if(declarations.length != 1)
			{
				return false;
			}
			final JSExpression rhs =
					declarations[0].getInitializer();
			if(rhs == null)
			{
				return false;
			}
			if(!(rhs instanceof JSReferenceExpression))
			{
				return false;
			}
			final PsiElement referent = ((PsiReference) rhs).resolve();
			if(referent == null || !referent.equals(variable))
			{
				return false;
			}
			for(int i = followingStatementNumber; i < statements.length; i++)
			{
				if(VariableAccessUtils.variableIsUsed(variable,
						statements[i]))
				{
					return false;
				}
			}
			return true;
		}

		private boolean variableIsUsedInInnerFunction(JSBlockStatement block,
													  JSVariable variable)
		{
			final VariableUsedInInnerFunctionVisitor visitor = new VariableUsedInInnerFunctionVisitor(variable);
			block.accept(visitor);
			return visitor.isUsedInInnerFunction();
		}
	}
}
