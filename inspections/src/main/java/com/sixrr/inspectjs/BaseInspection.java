package com.sixrr.inspectjs;

import java.lang.reflect.Method;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import org.jetbrains.annotations.NonNls;
import com.intellij.codeInspection.CustomSuppressableInspectionTool;
import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.codeInspection.SuppressIntentionAction;
import com.intellij.codeInspection.SuppressionUtil;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSSuppressionHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.util.PsiTreeUtil;

public abstract class BaseInspection extends LocalInspectionTool implements CustomSuppressableInspectionTool
{
	private final String m_shortName = null;

	@Override
	@Nonnull
	public String getShortName()
	{
		if(m_shortName == null)
		{
			final Class<? extends BaseInspection> aClass = getClass();
			@NonNls final String name = aClass.getName();
			assert name.endsWith("Inspection") : "class name must end with the 'Inspection' to correctly calculate the short name: " + name;
			return name.substring(name.lastIndexOf((int) '.') + 1, name.length() - "Inspection".length());
		}
		return m_shortName;
	}

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