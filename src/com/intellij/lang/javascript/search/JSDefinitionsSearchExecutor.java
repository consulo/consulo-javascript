package com.intellij.lang.javascript.search;

import com.intellij.lang.Language;
import com.intellij.lang.javascript.JavascriptLanguage;
import com.intellij.lang.javascript.index.JSNamedElementProxy;
import com.intellij.lang.javascript.index.JavaScriptIndex;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSDefinitionExpression;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.ResolveResult;
import com.intellij.psi.search.GlobalSearchScope;
import com.intellij.psi.search.searches.ReferencesSearch;
import com.intellij.util.Processor;
import com.intellij.util.QueryExecutor;

/**
 * @author Maxim.Mossienko
*         Date: Apr 28, 2008
*         Time: 8:34:30 PM
*/
class JSDefinitionsSearchExecutor implements QueryExecutor<PsiElement, PsiElement> {
  public boolean execute(final PsiElement sourceElement, final Processor<PsiElement> consumer) {
    if (sourceElement instanceof PsiNamedElement &&
        sourceElement.getLanguage().isKindOf(Language.findInstance(JavascriptLanguage.class))) {
      ReferencesSearch.search(sourceElement, GlobalSearchScope.projectScope(sourceElement.getProject()))
          .forEach(new Processor<PsiReference>() {
            public boolean process(final PsiReference t) {
              if (t instanceof JSReferenceExpression) {
                final PsiElement parent = ((JSReferenceExpression)t).getParent();
                final ResolveResult[] resolveResults = ((JSReferenceExpression)t).multiResolve(true);

                for (ResolveResult r : resolveResults) {
                  PsiElement psiElement = r.getElement();

                  if (psiElement != null &&
                      !JavaScriptIndex.isFromPredefinedFile(psiElement.getContainingFile()) &&
                      sourceElement != psiElement) {
                    if (psiElement instanceof JSNamedElementProxy) {
                      psiElement = ((JSNamedElementProxy)psiElement).getElement();
                    }

                    if ((psiElement instanceof JSFunction) && sourceElement instanceof JSFunction) {
                      JSFunction fun = (JSFunction)psiElement;
                      JSFunction sourceFun = (JSFunction)sourceElement;

                      if ((sourceFun.isGetProperty() && fun.isSetProperty()) || (sourceFun.isSetProperty() && fun.isGetProperty())) {
                        return true;
                      }
                    }

                    if ((psiElement != sourceElement || !(psiElement instanceof JSClass)) && !consumer.process(psiElement)) return false;
                  }
                }

                if (!(parent instanceof JSDefinitionExpression)) {
                  return false;
                }
              }
              return true;
            }
          });

      if (sourceElement instanceof JSClass) {
        final JSClass clazz = (JSClass)sourceElement;

        final Processor<JSClass> delegatingProcessor = new Processor<JSClass>() {
          public boolean process(final JSClass jsClass) {
            return consumer.process(jsClass);
          }
        };
        JSClassSearch.searchClassInheritors(clazz, true).forEach(delegatingProcessor);

        if (clazz.isInterface()) {
          JSClassSearch.searchInterfaceImplementations(clazz, true).forEach(delegatingProcessor);
        }
      }
      else if (sourceElement instanceof JSFunction) {
        final JSFunction baseFunction = (JSFunction)sourceElement;
        final Processor<JSFunction> delegatingProcessor = new Processor<JSFunction>() {
          public boolean process(final JSFunction jsFunction) {
            return consumer.process(jsFunction);
          }
        };
        JSFunctionsSearch.searchOverridingFunctions(baseFunction, true).forEach(delegatingProcessor);

        final PsiElement parent = baseFunction.getParent();
        if (parent instanceof JSClass && ((JSClass)parent).isInterface()) {
          JSFunctionsSearch.searchImplementingFunctions(baseFunction, true).forEach(delegatingProcessor);
        }
      }
    }
    return true;
  }
}
