/*
 * Copyright 2000-2006 JetBrains s.r.o.
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

package com.intellij.lang.javascript.inspections;

import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.codeHighlighting.HighlightDisplayLevel;
import com.intellij.codeInspection.CustomSuppressableInspectionTool;
import com.intellij.codeInspection.LocalInspectionTool;
import com.intellij.codeInspection.ProblemsHolder;
import com.intellij.codeInspection.SuppressIntentionAction;
import com.intellij.codeInspection.SuppressionUtil;
import com.intellij.lang.javascript.psi.JSElementVisitor;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSSuppressionHolder;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.util.PsiTreeUtil;

/**
 * Created by IntelliJ IDEA.
 * User: Maxim.Mossienko
 * Date: Apr 18, 2006
 * Time: 7:34:46 PM
 * To change this template use File | Settings | File Templates.
 */
public abstract class JSInspection extends LocalInspectionTool implements CustomSuppressableInspectionTool
{
	protected boolean myOnTheFly;

	protected abstract JSElementVisitor createVisitor(final ProblemsHolder holder);

	@NotNull
	public PsiElementVisitor buildVisitor(@NotNull ProblemsHolder holder, boolean isOnTheFly)
	{
		myOnTheFly = isOnTheFly;
		return createVisitor(holder);
	}

	public boolean isEnabledByDefault()
	{
		return true;
	}

	@NotNull
	public HighlightDisplayLevel getDefaultLevel()
	{
		return HighlightDisplayLevel.INFO;
	}

	public PsiNamedElement getProblemElement(final PsiElement psiElement)
	{
		return PsiTreeUtil.getNonStrictParentOfType(psiElement, JSNamedElement.class);
	}

	@Nullable
	public SuppressIntentionAction[] getSuppressActions(final PsiElement element)
	{
		return new SuppressIntentionAction[]{
	/*  new AddNoInspectionCommentFix(HighlightDisplayKey.find(getShortName()), JSSuppressionHolder.class),*/
		};
	}

	public boolean isSuppressedFor(final PsiElement element)
	{
		return SuppressionUtil.isSuppressedInStatement(element, getID(), JSSuppressionHolder.class);
	}
}
