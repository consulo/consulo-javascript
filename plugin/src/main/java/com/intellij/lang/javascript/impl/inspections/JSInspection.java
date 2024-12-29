/*
 * Copyright 2000-2005 JetBrains s.r.o.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.intellij.lang.javascript.impl.inspections;

import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSNamedElement;
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
import consulo.language.psi.PsiNamedElement;
import consulo.language.psi.util.PsiTreeUtil;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * @author Maxim.Mossienko
 * @since 2006-08-18
 */
public abstract class JSInspection extends LocalInspectionTool implements CustomSuppressableInspectionTool
{
	protected boolean myOnTheFly;

	protected abstract JSElementVisitor createVisitor(final ProblemsHolder holder);

	@Override
	@Nonnull
	public PsiElementVisitor buildVisitor(@Nonnull ProblemsHolder holder, boolean isOnTheFly)
	{
		myOnTheFly = isOnTheFly;
		return createVisitor(holder);
	}

	@Override
	public boolean isEnabledByDefault()
	{
		return true;
	}

	@Nullable
	@Override
	public Language getLanguage()
	{
		return JavaScriptLanguage.INSTANCE;
	}

	@Override
	@Nonnull
	public HighlightDisplayLevel getDefaultLevel()
	{
		return HighlightDisplayLevel.WEAK_WARNING;
	}

	@Override
	public PsiNamedElement getProblemElement(final PsiElement psiElement)
	{
		return PsiTreeUtil.getNonStrictParentOfType(psiElement, JSNamedElement.class);
	}

	@Override
	@Nullable
	public SuppressIntentionAction[] getSuppressActions(final PsiElement element)
	{
		return new SuppressIntentionAction[]{
	/*  new AddNoInspectionCommentFix(HighlightDisplayKey.find(getShortName()), JSSuppressionHolder.class),*/
		};
	}

	@Override
	public boolean isSuppressedFor(final PsiElement element)
	{
		return SuppressionUtil.isSuppressedInStatement(element, getID(), JSSuppressionHolder.class);
	}
}
