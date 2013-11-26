/*
 * @author max
 */
package com.intellij.lang.javascript.refactoring;

import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNamedElement;
import com.intellij.lang.javascript.psi.JSProperty;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.refactoring.extractMethod.JSExtractFunctionHandler;
import com.intellij.lang.javascript.refactoring.introduceConstant.JSIntroduceConstantHandler;
import com.intellij.lang.javascript.refactoring.introduceField.JSIntroduceFieldHandler;
import com.intellij.lang.javascript.refactoring.introduceVariable.JSIntroduceVariableHandler;
import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.LocalSearchScope;
import com.intellij.refactoring.RefactoringActionHandler;

public class JavascriptRefactoringSupportProvider extends RefactoringSupportProvider
{
  public boolean isSafeDeleteAvailable(PsiElement element) {
    boolean simpleElement =
      element instanceof JSFunction || element instanceof JSVariable || element instanceof JSDefinitionExpression ||
      element instanceof JSProperty || element instanceof JSClass;

    if (element instanceof JSNamedElementProxy) {
      final JSNamedElementProxy.NamedItemType namedItemType = ((JSNamedElementProxy)element).getType();

      simpleElement = namedItemType != JSNamedElementProxy.NamedItemType.AttributeValue;
    }

    return simpleElement && ((JSNamedElement)element).getName() != null;
  }

  @Nullable
  public RefactoringActionHandler getIntroduceVariableHandler() {
    return new JSIntroduceVariableHandler();
  }

  @Nullable
  public RefactoringActionHandler getExtractMethodHandler() {
    return ApplicationManager.getApplication().isUnitTestMode() ?new JSExtractFunctionHandler():null;
  }

  @Override
  public RefactoringActionHandler getIntroduceConstantHandler() {
    return new JSIntroduceConstantHandler();
  }

  @Override
  public RefactoringActionHandler getIntroduceFieldHandler() {
    return new JSIntroduceFieldHandler();
  }

  public boolean doInplaceRenameFor(final PsiElement element, final PsiElement context) {
    return element instanceof JSNamedElement && element.getUseScope() instanceof LocalSearchScope;
  }
}