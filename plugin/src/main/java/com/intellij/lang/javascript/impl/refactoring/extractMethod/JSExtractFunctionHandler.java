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

package com.intellij.lang.javascript.impl.refactoring.extractMethod;

import consulo.codeEditor.Editor;
import consulo.dataContext.DataContext;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.refactoring.action.RefactoringActionHandler;
import consulo.language.editor.refactoring.util.CommonRefactoringUtil;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.project.Project;
import consulo.ui.ex.awt.DialogWrapper;
import consulo.undoRedo.CommandProcessor;

import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 *         Date: May 29, 2008
 *         Time: 8:19:48 PM
 */
public class JSExtractFunctionHandler implements RefactoringActionHandler
{
	@Override
	public void invoke(@Nonnull final Project project, final Editor editor, PsiFile file, DataContext dataContext)
	{
		if(!editor.getSelectionModel().hasSelection())
		{
			editor.getSelectionModel().selectLineAtCaret();
		}
		int start = editor.getSelectionModel().getSelectionStart();
		int end = editor.getSelectionModel().getSelectionEnd();

		if(!CommonRefactoringUtil.checkReadOnlyStatus(project, file))
		{
			return;
		}

		final JSExtractFunctionSettings settings = getSettings(project, editor);
		if(settings == null)
		{
			return;
		}

		CommandProcessor.getInstance().executeCommand(
			project,
			() -> project.getApplication().runWriteAction(() -> doRefactoring(project, editor, settings)),
			JavaScriptLocalize.javascriptExtractMethodTitle().get(),
			null
		);
	}

	protected JSExtractFunctionSettings getSettings(final Project project, final Editor editor)
	{
		final JSExtractFunctionDialog dialog = new JSExtractFunctionDialog();
		dialog.show();
		if(dialog.getExitCode() != DialogWrapper.OK_EXIT_CODE)
		{
			return null;
		}
		return dialog;
	}

	private void doRefactoring(final Project project, final Editor editor, final JSExtractFunctionSettings settings)
	{
	}

	@Override
	public void invoke(@Nonnull final Project project, @Nonnull final PsiElement[] elements, final DataContext dataContext)
	{
		throw new UnsupportedOperationException();
	}
}
