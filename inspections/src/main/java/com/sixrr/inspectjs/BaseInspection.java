package com.sixrr.inspectjs;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSSuppressionHolder;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.inspection.CustomSuppressableInspectionTool;
import consulo.language.editor.inspection.LocalInspectionTool;
import consulo.language.editor.inspection.ProblemsHolder;
import consulo.language.editor.inspection.SuppressionUtil;
import consulo.language.editor.intention.SuppressIntentionAction;
import consulo.language.editor.rawHighlight.HighlightDisplayLevel;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiElementVisitor;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;
import java.lang.reflect.Method;

public abstract class BaseInspection extends LocalInspectionTool implements CustomSuppressableInspectionTool
{
	@Override
	@Nonnull
	public PsiElementVisitor buildVisitor(@Nonnull ProblemsHolder problemsHolder, boolean onTheFly)
	{
		if(!canBuildVisitor(problemsHolder.getFile()))
		{
			return new PsiElementVisitor()
			{
			};
		}
		final BaseInspectionVisitor visitor = buildVisitor();
		visitor.setProblemsHolder(problemsHolder);
		visitor.setOnTheFly(onTheFly);
		visitor.setInspection(this);
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
	protected String buildErrorString(Object... args)
	{
		return null;
	}

	protected boolean buildQuickFixesOnlyForOnTheFlyErrors()
	{
		return false;
	}

	@Nullable
	protected InspectionJSFix buildFix(PsiElement location)
	{
		return null;
	}

	@Nullable
	protected InspectionJSFix[] buildFixes(PsiElement location)
	{
		return null;
	}

	public boolean hasQuickFix()
	{
		final Method[] methods = getClass().getDeclaredMethods();
		for(final Method method : methods)
		{
			@NonNls final String methodName = method.getName();
			if("buildFix".equals(methodName))
			{
				return true;
			}
		}
		return false;
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