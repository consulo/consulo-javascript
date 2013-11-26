package com.intellij.lang.javascript.generation;

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
import com.intellij.codeInsight.actions.BaseCodeInsightAction;
import org.jetbrains.annotations.NotNull;

/**
 * @author Maxim.Mossienko
 *         Date: Jul 19, 2008
 *         Time: 7:04:37 PM
 */
abstract class BaseJSGenerateAction extends AnAction {
  public void actionPerformed(final AnActionEvent e) {
    Editor editor = e.getData(PlatformDataKeys.EDITOR);
    PsiFile psifile = e.getData(LangDataKeys.PSI_FILE);
    Project project = e.getData(PlatformDataKeys.PROJECT);

    final VirtualFile file = e.getData(PlatformDataKeys.VIRTUAL_FILE);
    if (JavaScriptSupportLoader.isFlexMxmFile(file)) {
      editor = BaseCodeInsightAction.getInjectedEditor(project, editor);
      psifile = PsiUtilBase.getPsiFileInEditor(editor, project);
    }

    new JavaScriptGenerateAccessorHandler(getGenerationMode()).invoke(
      project, editor, psifile
    );
  }

  protected abstract @NotNull JavaScriptGenerateAccessorHandler.GenerationMode getGenerationMode();

  @Override
  public void update(final AnActionEvent e) {
    final VirtualFile file = e.getData(PlatformDataKeys.VIRTUAL_FILE);

    boolean status = false;

    if (file != null) {
      if (file.getFileType() == JavaScriptSupportLoader.JAVASCRIPT) {
        final Editor editor = e.getData(PlatformDataKeys.EDITOR);
        final PsiFile psifile = e.getData(LangDataKeys.PSI_FILE);

        if (editor != null && psifile != null) {
          status = psifile.getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4;
        }
      } else if (JavaScriptSupportLoader.isFlexMxmFile(file)) {
        status = true;
      }
    }

    e.getPresentation().setEnabled(status);
    e.getPresentation().setVisible(status);
  }
}
