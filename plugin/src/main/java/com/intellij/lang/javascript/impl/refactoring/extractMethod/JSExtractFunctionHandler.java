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
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.ex.awt.DialogWrapper;
import consulo.undoRedo.CommandProcessor;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 * @since 2008-05-29
 */
public class JSExtractFunctionHandler implements RefactoringActionHandler {
    @Override
    @RequiredUIAccess
    public void invoke(@Nonnull final Project project, final Editor editor, PsiFile file, DataContext dataContext) {
        if (!editor.getSelectionModel().hasSelection()) {
            editor.getSelectionModel().selectLineAtCaret();
        }

        if (!CommonRefactoringUtil.checkReadOnlyStatus(project, file)) {
            return;
        }

        JSExtractFunctionSettings settings = getSettings(project, editor);
        if (settings == null) {
            return;
        }

        CommandProcessor.getInstance().newCommand()
            .project(project)
            .name(JavaScriptLocalize.javascriptExtractMethodTitle())
            .run(() -> project.getApplication().runWriteAction(() -> doRefactoring(project, editor, settings)));
    }

    @RequiredUIAccess
    protected JSExtractFunctionSettings getSettings(Project project, Editor editor) {
        JSExtractFunctionDialog dialog = new JSExtractFunctionDialog();
        dialog.show();
        if (dialog.getExitCode() != DialogWrapper.OK_EXIT_CODE) {
            return null;
        }
        return dialog;
    }

    private void doRefactoring(Project project, Editor editor, JSExtractFunctionSettings settings) {
    }

    @Override
    @RequiredUIAccess
    public void invoke(@Nonnull Project project, @Nonnull PsiElement[] elements, DataContext dataContext) {
        throw new UnsupportedOperationException();
    }
}
