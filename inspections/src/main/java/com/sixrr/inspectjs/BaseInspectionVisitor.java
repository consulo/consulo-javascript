package com.sixrr.inspectjs;

import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.editor.inspection.*;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiWhiteSpace;
import consulo.language.psi.util.PsiTreeUtil;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.util.List;

public abstract class BaseInspectionVisitor<T> extends JSElementVisitor
{
	private BaseInspection inspection = null;
	private ProblemsHolder problemsHolder = null;
	private boolean onTheFly = false;
	private List<ProblemDescriptor> errors = null;
	protected T myState;

	public void setState(T state)
	{
		myState = state;
	}

	public void setInspection(BaseInspection inspection)
	{
		this.inspection = inspection;
	}

	public void setProblemsHolder(ProblemsHolder problemsHolder)
	{
		this.problemsHolder = problemsHolder;
	}

	public void setOnTheFly(boolean onTheFly)
	{
		this.onTheFly = onTheFly;
	}

	@RequiredReadAction
	protected void registerFunctionCallError(JSCallExpression expression)
	{
		final JSExpression methodExpression = expression.getMethodExpression();
		PsiElement errorLocation = null;

		if(methodExpression instanceof JSReferenceExpression)
		{
			errorLocation = ((JSReferenceExpression) methodExpression).getReferenceNameElement();
		}
		else if(methodExpression instanceof JSFunction)
		{
			final PsiElement node = ((JSFunction) methodExpression).getNameIdentifier();
			if(node != null)
			{
				errorLocation = node;
			}
			else
			{
				errorLocation = methodExpression;
			}
		}

		registerError(errorLocation);
	}

	protected void registerStatementError(JSStatement statement, Object... args)
	{
		final PsiElement statementToken = statement.getFirstChild();
		registerError(statementToken, args);
	}

	protected void registerFunctionError(JSFunction function)
	{
		final PsiElement identifier = function.getNameIdentifier();
		if(identifier == null ||
				!PsiTreeUtil.isAncestor(function, identifier, true))
		{
			registerError(function.getFirstChild());
		}
		else
		{
			registerError(identifier);
		}
	}

	protected void registerVariableError(JSVariable variable)
	{
		final PsiElement nameIdentifier = variable.getFirstChild();
		registerError(nameIdentifier);
	}

	protected void registerError(PsiElement location)
	{
		if(location == null)
		{
			return;
		}
		final LocalQuickFix[] fix = createFixes(location, myState);
		final String description = inspection.buildErrorString(myState, location);
		registerError(location, description, fix);
	}

	private void registerError(PsiElement location, String description,
							   LocalQuickFix[] fixes)
	{
		problemsHolder.registerProblem(getEditorErrorLocation(location),
				description,
				getProblemHighlightType(location), fixes);
	}

	protected ProblemHighlightType getProblemHighlightType(PsiElement location)
	{
		return ProblemHighlightType.GENERIC_ERROR_OR_WARNING;
	}

	protected PsiElement getEditorErrorLocation(final @Nonnull PsiElement location)
	{
		return location;
	}

	protected void registerError(PsiElement location, Object... args)
	{
		final LocalQuickFix[] fix = createFixes(location, myState);
		final String description = inspection.buildErrorString(myState, args);
		registerError(location, description, fix);
	}

	@Nullable
	private LocalQuickFix[] createFixes(PsiElement location, Object state)
	{
		if(!onTheFly && inspection.buildQuickFixesOnlyForOnTheFlyErrors())
		{
			return null;
		}
		final InspectionJSFix[] fixes = inspection.buildFixes(location);
		if(fixes != null)
		{
			return fixes;
		}
		final InspectionJSFix fix = inspection.buildFix(location, state);
		if(fix == null)
		{
			return null;
		}
		return new InspectionJSFix[]{fix};
	}

	@Nullable
	public ProblemDescriptor[] getErrors()
	{
		if(errors == null)
		{
			return null;
		}
		else
		{
			final int numErrors = errors.size();
			return errors.toArray(new ProblemDescriptor[numErrors]);
		}
	}

	@Override
	public void visitWhiteSpace(PsiWhiteSpace space)
	{
		// none of our inspections need to do anything with white space,
		// so this is a performance optimization
	}
}
