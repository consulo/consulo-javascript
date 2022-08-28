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

/*
 * @author max
 */
package com.intellij.lang.javascript.impl.refactoring;

import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.impl.refactoring.extractMethod.JSExtractFunctionHandler;
import com.intellij.lang.javascript.impl.refactoring.introduceConstant.JSIntroduceConstantHandler;
import com.intellij.lang.javascript.impl.refactoring.introduceField.JSIntroduceFieldHandler;
import com.intellij.lang.javascript.impl.refactoring.introduceVariable.JSIntroduceVariableHandler;
import consulo.application.ApplicationManager;
import consulo.language.editor.refactoring.RefactoringSupportProvider;
import consulo.language.editor.refactoring.action.RefactoringActionHandler;
import consulo.language.psi.PsiElement;
import consulo.language.psi.scope.LocalSearchScope;

import javax.annotation.Nullable;

public abstract class JavascriptRefactoringSupportProvider extends RefactoringSupportProvider
{
	@Override
	public boolean isSafeDeleteAvailable(PsiElement element)
	{
		boolean simpleElement = element instanceof JSFunction || element instanceof JSVariable || element instanceof JSDefinitionExpression ||
				element instanceof JSProperty || element instanceof JSClass;

		return simpleElement && ((JSNamedElement) element).getName() != null;
	}

	@Override
	@Nullable
	public RefactoringActionHandler getIntroduceVariableHandler()
	{
		return new JSIntroduceVariableHandler();
	}

	@Override                                                                                
	@Nullable
	public RefactoringActionHandler getExtractMethodHandler()
	{
		return ApplicationManager.getApplication().isUnitTestMode() ? new JSExtractFunctionHandler() : null;
	}

	@Override
	public RefactoringActionHandler getIntroduceConstantHandler()
	{
		return new JSIntroduceConstantHandler();
	}

	@Override
	public RefactoringActionHandler getIntroduceFieldHandler()
	{
		return new JSIntroduceFieldHandler();
	}

	public boolean doInplaceRenameFor(final PsiElement element, final PsiElement context)
	{
		return element instanceof JSNamedElement && element.getUseScope() instanceof LocalSearchScope;
	}
}