package com.sixrr.inspectjs;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSSuppressionHolder;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.inspection.*;
import consulo.language.editor.intention.SuppressIntentionAction;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.util.PsiTreeUtil;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

public abstract class BaseInspection extends LocalInspectionTool implements CustomSuppressableInspectionTool
{
	@Override
	@Nonnull
	@SuppressWarnings("unchecked")
	public PsiElementVisitor buildVisitor(@Nonnull ProblemsHolder problemsHolder, boolean onTheFly, LocalInspectionToolSession session, Object state)
	{
		if(!canBuildVisitor(problemsHolder.getFile()))
		{
			return PsiElementVisitor.EMPTY_VISITOR;
		}

		final BaseInspectionVisitor visitor = buildVisitor();
		visitor.setProblemsHolder(problemsHolder);
		visitor.setOnTheFly(onTheFly);
		visitor.setInspection(this);
		visitor.setState(state);
		return visitor;
	}

	@Nullable
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}

	@Nonnull
	@Override
	public HighlightDisplayLevel getDefaultLevel()
	{
		return HighlightDisplayLevel.WARNING;
	}

	public boolean canBuildVisitor(@Nonnull PsiFile psiFile)
	{
		return true;
	}

	@Nullable
	@RequiredReadAction
	protected String buildErrorString(Object state, Object... args)
	{
		return null;
	}

	protected boolean buildQuickFixesOnlyForOnTheFlyErrors()
	{
		return false;
	}

	@Nullable
	protected InspectionJSFix buildFix(PsiElement location, Object state)
	{
		return null;
	}

	@Nullable
	protected InspectionJSFix[] buildFixes(PsiElement location)
	{
		return null;
	}

	public abstract BaseInspectionVisitor buildVisitor();

	protected boolean functionHasIdentifier(JSFunction function)
	{
		final PsiElement identifier = function.getNameIdentifier();
		return identifier != null && PsiTreeUtil.isAncestor(function, identifier, true);
	}

	@Override
	public PsiNamedElement getProblemElement(final PsiElement psiElement)
	{
		return PsiTreeUtil.getNonStrictParentOfType(psiElement, PsiFile.class);
	}

	@Override
	public SuppressIntentionAction[] getSuppressActions(@Nullable final PsiElement element)
	{
		return new SuppressIntentionAction[]{
		   /* new AddNoInspectionCommentFix(HighlightDisplayKey.find(getShortName()), JSSuppressionHolder.class),  */
		};
	}

	@Override
	public boolean isSuppressedFor(@Nonnull final PsiElement element)
	{
		return SuppressionUtil.isSuppressedInStatement(element, getID(), JSSuppressionHolder.class);
	}
}