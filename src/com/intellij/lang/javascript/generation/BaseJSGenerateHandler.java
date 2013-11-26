/*
 * @author max
 */
package com.intellij.lang.javascript.generation;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

import javax.swing.JComponent;

import org.jetbrains.annotations.NonNls;
import com.intellij.ide.util.MemberChooser;
import com.intellij.lang.LanguageCodeInsightActionHandler;
import com.intellij.lang.javascript.JSBundle;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.validation.BaseCreateMethodsFix;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.command.CommandProcessor;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.DialogWrapper;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;

abstract class BaseJSGenerateHandler implements LanguageCodeInsightActionHandler {
  public void invoke(final Project project, final Editor editor, final PsiFile file) {
    JSClass clazz = findClass(file, editor);

    if (clazz == null) return;

    final Collection<JSNamedElementNode> candidates = new ArrayList<JSNamedElementNode>();
    collectCandidates(clazz, candidates);

    final Collection<JSNamedElementNode> selectedElements;

    if (ApplicationManager.getApplication().isUnitTestMode()) {
      selectedElements = candidates;
    } else {
      if (candidates.size() == 0) {
        if (!canHaveEmptySelectedElements()) return;
        selectedElements = Collections.emptyList();
      } else {
        final MemberChooser<JSNamedElementNode> chooser = new MemberChooser<JSNamedElementNode>(
            candidates.toArray(new JSNamedElementNode[candidates.size()]), false, true, project, false
        ) {
          /*@Override
          protected void customizeOptionsPanel() {

			super.customizeOptionsPanel();

            appendOwnOptions(jComponentList);
            return jComponentList;
          } */
        };

        chooser.setTitle(JSBundle.message(getTitleKey()));
        chooser.setCopyJavadocVisible(false);
        chooser.show();
        if (chooser.getExitCode() != DialogWrapper.OK_EXIT_CODE) return;
        selectedElements = chooser.getSelectedElements();
      }
    }

    if (selectedElements == null) return;

    final JSClass jsClass = clazz;
    final Collection<JSNamedElementNode> selectedElements1 = selectedElements;
    Runnable runnable = new Runnable() {
      public void run() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
          public void run() {
            try {
              final BaseCreateMethodsFix createMethodsFix = createFix(jsClass);
              createMethodsFix.addElementsToProcessFrom(selectedElements1);
              createMethodsFix.invoke(project, editor, file);
            } catch (IncorrectOperationException ex) {
              Logger.getInstance(getClass().getName()).error(ex);
            }
          }
        });
      }
    };

    if (CommandProcessor.getInstance().getCurrentCommand() == null) {
      CommandProcessor.getInstance().executeCommand(project, runnable, getClass().getName(), null);
    } else {
      runnable.run();
    }
  }

  protected void appendOwnOptions(List<JComponent> jComponentList) {
  }

  protected boolean canHaveEmptySelectedElements() {
    return false;
  }

  static JSClass findClass(PsiFile file, Editor editor) {
    final PsiElement at = file.findElementAt(editor.getCaretModel().getOffset());
    if (at == null) return null;

    JSClass clazz = PsiTreeUtil.getParentOfType(at, JSClass.class);
    if (clazz == null) {
      final PsiFile containingFile = at.getContainingFile();
      final PsiElement element = JSResolveUtil.getClassReferenceForXmlFromContext(containingFile);
      if (element instanceof JSClass) clazz = (JSClass)element;
    } else if (JSResolveUtil.isArtificialClassUsedForReferenceList(clazz)) {
      clazz = null;
    }

    return clazz;
  }

  protected abstract /*@PropertyKey(resourceBundle = JSBundle.BUNDLE)*/ @NonNls String getTitleKey();

  protected abstract BaseCreateMethodsFix createFix(JSClass clazz);

  protected abstract void collectCandidates(final JSClass clazz, final Collection<JSNamedElementNode> candidates);

  public boolean startInWriteAction() {
    return false;
  }

  public boolean isValidFor(final Editor editor, final PsiFile file) {
    return true;
  }
}