package com.intellij.lang.javascript.refactoring.extractMethod;

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
import com.intellij.lang.javascript.JSBundle;
import org.jetbrains.annotations.NotNull;

/**
 * @author Maxim.Mossienko
 *         Date: May 29, 2008
 *         Time: 8:19:48 PM
 */
public class JSExtractFunctionHandler implements RefactoringActionHandler {
  public void invoke(@NotNull final Project project, final Editor editor, PsiFile file, DataContext dataContext) {
    if (!editor.getSelectionModel().hasSelection()) editor.getSelectionModel().selectLineAtCaret();
    int start = editor.getSelectionModel().getSelectionStart();
    int end = editor.getSelectionModel().getSelectionEnd();

    if (!CommonRefactoringUtil.checkReadOnlyStatus(project, file)) return;

    final JSExtractFunctionSettings settings = getSettings(project, editor);
    if (settings == null) return;

    CommandProcessor.getInstance().executeCommand(project, new Runnable() {
      public void run() {
        ApplicationManager.getApplication().runWriteAction(
          new Runnable() {
            public void run() {
              doRefactoring(project, editor, settings);
            }
          }
        );
      }
    }, JSBundle.message("javascript.extract.method.title"), null);
  }

  protected JSExtractFunctionSettings getSettings(final Project project, final Editor editor) {
    final JSExtractFunctionDialog dialog = new JSExtractFunctionDialog();
    dialog.show();
    if (dialog.getExitCode() != DialogWrapper.OK_EXIT_CODE) return null;
    return dialog;
  }

  private void doRefactoring(final Project project, final Editor editor, final JSExtractFunctionSettings settings) {
  }

  public void invoke(@NotNull final Project project, @NotNull final PsiElement[] elements, final DataContext dataContext) {
    throw new UnsupportedOperationException();
  }
}
