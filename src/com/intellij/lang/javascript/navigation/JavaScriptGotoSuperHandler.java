/*
 * @author max
 */
package com.intellij.lang.javascript.navigation;

import com.intellij.codeInsight.navigation.NavigationUtil;
import com.intellij.lang.LanguageCodeInsightActionHandler;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.navigation.NavigationItem;

public class JavaScriptGotoSuperHandler implements LanguageCodeInsightActionHandler {
  public void invoke(final Project project, final Editor editor, final PsiFile file) {
    final PsiElement at = file.findElementAt(editor.getCaretModel().getOffset());
    if (at == null) return;
    JSNamedElement namedElement = PsiTreeUtil.getParentOfType(at, JSNamedElement.class);
    PsiElement parent = namedElement != null ? namedElement.getParent():null;

    if (namedElement instanceof JSDefinitionExpression) {
      if (parent instanceof JSAssignmentExpression) {
        PsiElement rOperand = ((JSAssignmentExpression)parent).getROperand();
        if (rOperand instanceof JSFunctionExpression) {
          namedElement = (JSNamedElement)rOperand;
        }
      }
    }

    if (namedElement instanceof JSFunction) {
      final JSFunction function = (JSFunction)namedElement;
      final String qName = JSResolveUtil.getQNameToStartHierarchySearch(function);

      if (qName != null) {
        if (parent instanceof JSFile) {
          JSClass xmlBackedClass = JSResolveUtil.getXmlBackedClass((JSFile)parent);
          if (xmlBackedClass != null) {
            parent = xmlBackedClass;
          }
        }
        boolean result = JSResolveUtil.iterateType(function,
          parent instanceof JSClass ? parent:parent.getContainingFile(),
          qName,
          new JSResolveUtil.OverrideHandler() {
            public boolean process(final ResolveProcessor processor, final PsiElement scope, final String className) {
              ((NavigationItem)processor.getResult()).navigate(true);
              return false;
            }
          }
        );

        if (!result) return;
      }

      if (parent instanceof JSClass) {
        JSResolveUtil.processInterfaceMethods((JSClass)parent, new JSResolveUtil.CollectMethodsToImplementProcessor(function.getName(), function) {
          protected boolean process(final ResolveProcessor processor) {
            ((NavigationItem)processor.getResult()).navigate(true);
            return true;
          }
        });
      }
    } else if (namedElement instanceof JSClass) {
      final JSClass clazz = (JSClass)namedElement;
      final JSClass[] classes = clazz.getSupers();
      
      if (classes.length == 0) return;
      if (classes.length == 1) classes[0].navigate(true);
      else {
        NavigationUtil.getPsiElementPopup(classes, "Choose super class or interface").showInBestPositionFor(editor);
      }
    }
  }

  public boolean startInWriteAction() {
    return false;
  }

  public boolean isValidFor(final Editor editor, final PsiFile file) {
    return true;
  }
}
