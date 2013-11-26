/*
 * Copyright (c) 2000-2005 by JetBrains s.r.o. All Rights Reserved.
 * Use is subject to license terms.
 */
package com.intellij.javascript;

import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.lang.Language;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.util.Computable;
import com.intellij.psi.util.PsiUtilBase;
import com.intellij.util.Consumer;

/**
 * @author peter
 */
public class JSCompletionContributor extends CompletionContributor{
  private static boolean ourDoingSmartCodeCompleteAction;

  public static boolean isDoingSmartCodeCompleteAction() {
    return ourDoingSmartCodeCompleteAction;
  }

  @Override
  public boolean fillCompletionVariants(final CompletionParameters parameters, final CompletionResultSet result) {
    ourDoingSmartCodeCompleteAction = parameters.getCompletionType() == CompletionType.SMART &&
                                      getElementLanguage(parameters).isKindOf(JavaScriptSupportLoader.JAVASCRIPT.getLanguage());
    if (ourDoingSmartCodeCompleteAction) {
      final CompletionParameters newParams = new CompletionParameters(parameters.getPosition(), parameters.getOriginalFile(),
                                                                      CompletionType.BASIC, parameters.getOffset(),
                                                                      parameters.getInvocationCount());
      CompletionService.getCompletionService().getVariantsFromContributors(EP_NAME, newParams, this, new Consumer<LookupElement>() {
        public void consume(final LookupElement lookupElement) {
          result.addElement(lookupElement);
        }
      });
    }
    return true;
  }

  private static Language getElementLanguage(final CompletionParameters parameters) {
    return ApplicationManager.getApplication().runReadAction(new Computable<Language>() {
      public Language compute() {
        return PsiUtilBase.getLanguageAtOffset(parameters.getPosition().getContainingFile(), parameters.getOffset());
      }
    });
  }
}
