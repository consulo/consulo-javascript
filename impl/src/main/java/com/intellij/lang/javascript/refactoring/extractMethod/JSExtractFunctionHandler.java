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

package com.intellij.lang.javascript.refactoring.extractMethod;

import org.jetbrains.annotations.NotNull;
import com.intellij.lang.javascript.JavaScriptBundle;
import com.intellij.openapi.actionSystem.DataContext;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.refactoring.RefactoringActionHandler;
import com.intellij.refactoring.util.CommonRefactoringUtil;

/**
 * @author Maxim.Mossienko
 *         Date: May 29, 2008
 *         Time: 8:19:48 PM
 */
public class JSExtractFunctionHandler implements RefactoringActionHandler
{
	@Override
	public void invoke(@NotNull final Project project, final Editor editor, PsiFile file, DataContext dataContext)
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

		CommandProcessor.getInstance().executeCommand(project, new Runnable()
		{
			@Override
			public void run()
			{
				ApplicationManager.getApplication().runWriteAction(new Runnable()
				{
					@Override
					public void run()
					{
						doRefactoring(project, editor, settings);
					}
				});
			}
		}, JavaScriptBundle.message("javascript.extract.method.title"), null);
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
	public void invoke(@NotNull final Project project, @NotNull final PsiElement[] elements, final DataContext dataContext)
	{
		throw new UnsupportedOperationException();
	}
}
