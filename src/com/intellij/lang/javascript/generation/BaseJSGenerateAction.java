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

package com.intellij.lang.javascript.generation;

import org.jetbrains.annotations.NotNull;
import com.intellij.codeInsight.actions.BaseCodeInsightAction;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.openapi.actionSystem.AnAction;
import com.intellij.openapi.actionSystem.AnActionEvent;
import com.intellij.openapi.actionSystem.LangDataKeys;
import com.intellij.openapi.actionSystem.PlatformDataKeys;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiUtilBase;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 19, 2008
 *         Time: 7:04:37 PM
 */
abstract class BaseJSGenerateAction extends AnAction
{
	@Override
	public void actionPerformed(final AnActionEvent e)
	{
		Editor editor = e.getData(PlatformDataKeys.EDITOR);
		PsiFile psifile = e.getData(LangDataKeys.PSI_FILE);
		Project project = e.getData(PlatformDataKeys.PROJECT);

		final VirtualFile file = e.getData(PlatformDataKeys.VIRTUAL_FILE);
		if(JavaScriptSupportLoader.isFlexMxmFile(file))
		{
			editor = BaseCodeInsightAction.getInjectedEditor(project, editor);
			psifile = PsiUtilBase.getPsiFileInEditor(editor, project);
		}

		new JavaScriptGenerateAccessorHandler(getGenerationMode()).invoke(project, editor, psifile);
	}

	protected abstract
	@NotNull
	JavaScriptGenerateAccessorHandler.GenerationMode getGenerationMode();

	@Override
	public void update(final AnActionEvent e)
	{
		final VirtualFile file = e.getData(PlatformDataKeys.VIRTUAL_FILE);

		boolean status = false;

		if(file != null)
		{
			if(file.getFileType() == JavaScriptSupportLoader.JAVASCRIPT)
			{
				final Editor editor = e.getData(PlatformDataKeys.EDITOR);
				final PsiFile psifile = e.getData(LangDataKeys.PSI_FILE);

				if(editor != null && psifile != null)
				{
					status = psifile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;
				}
			}
			else if(JavaScriptSupportLoader.isFlexMxmFile(file))
			{
				status = true;
			}
		}

		e.getPresentation().setEnabled(status);
		e.getPresentation().setVisible(status);
	}
}
