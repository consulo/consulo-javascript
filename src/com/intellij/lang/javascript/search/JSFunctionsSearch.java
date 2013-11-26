package com.intellij.lang.javascript.search;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.psi.PsiElement;
import com.intellij.util.Processor;
import com.intellij.util.Query;
import com.intellij.util.QueryExecutor;
import com.intellij.util.QueryFactory;

public abstract class JSFunctionsSearch implements QueryExecutor<JSFunction, JSFunctionsSearch.SearchParameters> {
  public static class SearchParameters {
    private JSFunction myBaseFunction;
    private boolean myCheckDeepInheritance;

    public SearchParameters(final JSFunction baseFunction, final boolean checkDeepInheritance) {
      myBaseFunction = baseFunction;
      myCheckDeepInheritance = checkDeepInheritance;
    }

    public JSFunction getBaseFunction() {
      return myBaseFunction;
    }

    public boolean isCheckDeepInheritance() {
      return myCheckDeepInheritance;
    }
  }

  public static Query<JSFunction> searchOverridingFunctions(final JSFunction baseFunction, final boolean checkDeepInheritance) {
    final SearchParameters parameters = new SearchParameters(baseFunction, checkDeepInheritance);
    return OVERRIDDEN_FUNCTIONS_QUERY_FACTORY.createUniqueResultsQuery(parameters);
  }

  public static Query<JSFunction> searchImplementingFunctions(final JSFunction baseFunction, final boolean checkDeepInheritance) {
    final SearchParameters parameters = new SearchParameters(baseFunction, checkDeepInheritance);
    return IMPLEMENTING_FUNCTIONS_QUERY_FACTORY.createUniqueResultsQuery(parameters);
  }

  private static QueryFactory<JSFunction, SearchParameters> OVERRIDDEN_FUNCTIONS_QUERY_FACTORY = new QueryFactory<JSFunction, SearchParameters>();
  private static QueryFactory<JSFunction, SearchParameters> IMPLEMENTING_FUNCTIONS_QUERY_FACTORY = new QueryFactory<JSFunction, SearchParameters>();

  static {
    OVERRIDDEN_FUNCTIONS_QUERY_FACTORY.registerExecutor(new JSFunctionsSearch() {
      protected Query<JSClass> makeQuery(final SearchParameters queryParameters, final PsiElement parent) {
        return JSClassSearch.searchClassInheritors((JSClass)parent, queryParameters.isCheckDeepInheritance());
      }
    });

    IMPLEMENTING_FUNCTIONS_QUERY_FACTORY.registerExecutor(new ImplementingFunctionsSearch());
  }

  public boolean execute(final SearchParameters queryParameters, final Processor<JSFunction> consumer) {
    final JSFunction baseFunction = queryParameters.getBaseFunction();
    PsiElement clazz = JSResolveUtil.findParent(baseFunction);
    
    if (!(clazz instanceof JSClass))  {
      return true;
    }

    return makeQuery(queryParameters, clazz).forEach(new Processor<JSClass>() {
      public boolean process(final JSClass jsClass) {
        JSFunction function = jsClass.findFunctionByNameAndKind(baseFunction.getName(), baseFunction.getKind());
        if (function != null) return consumer.process(function);
        return true;
      }
    });
  }

  protected abstract Query<JSClass> makeQuery(final SearchParameters queryParameters, final PsiElement parent);

  private static class ImplementingFunctionsSearch extends JSFunctionsSearch {
    protected Query<JSClass> makeQuery(final SearchParameters queryParameters, final PsiElement parent) {
      return JSClassSearch.searchInterfaceImplementations((JSClass)parent, true);
    }
  }
}
