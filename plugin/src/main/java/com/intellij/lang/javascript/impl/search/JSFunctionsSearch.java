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

package com.intellij.lang.javascript.impl.search;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.search.JSClassSearch;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.util.query.Query;
import consulo.application.util.query.QueryExecutor;
import consulo.application.util.query.QueryFactory;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;

import java.util.function.Predicate;

public abstract class JSFunctionsSearch implements QueryExecutor<JSFunction, JSFunctionsSearch.SearchParameters> {
    public static class SearchParameters {
        private JSFunction myBaseFunction;
        private boolean myCheckDeepInheritance;

        public SearchParameters(JSFunction baseFunction, boolean checkDeepInheritance) {
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

    public static Query<JSFunction> searchOverridingFunctions(JSFunction baseFunction, boolean checkDeepInheritance) {
        SearchParameters parameters = new SearchParameters(baseFunction, checkDeepInheritance);
        return OVERRIDDEN_FUNCTIONS_QUERY_FACTORY.createUniqueResultsQuery(parameters);
    }

    public static Query<JSFunction> searchImplementingFunctions(JSFunction baseFunction, boolean checkDeepInheritance) {
        SearchParameters parameters = new SearchParameters(baseFunction, checkDeepInheritance);
        return IMPLEMENTING_FUNCTIONS_QUERY_FACTORY.createUniqueResultsQuery(parameters);
    }

    private static QueryFactory<JSFunction, SearchParameters> OVERRIDDEN_FUNCTIONS_QUERY_FACTORY = new QueryFactory<>();
    private static QueryFactory<JSFunction, SearchParameters> IMPLEMENTING_FUNCTIONS_QUERY_FACTORY = new QueryFactory<>();

    static {
        OVERRIDDEN_FUNCTIONS_QUERY_FACTORY.registerExecutor(new JSFunctionsSearch() {
            @Override
            protected Query<JSClass> makeQuery(SearchParameters queryParameters, PsiElement parent) {
                return JSClassSearch.searchClassInheritors((JSClass)parent, queryParameters.isCheckDeepInheritance());
            }
        });

        IMPLEMENTING_FUNCTIONS_QUERY_FACTORY.registerExecutor(new ImplementingFunctionsSearch());
    }

    @Override
    public boolean execute(SearchParameters queryParameters, final @Nonnull Predicate<? super JSFunction> consumer) {
        final JSFunction baseFunction = queryParameters.getBaseFunction();
        PsiElement clazz = JSResolveUtil.findParent(baseFunction);

        //noinspection SimplifiableIfStatement
        if (!(clazz instanceof JSClass)) {
            return true;
        }

        return makeQuery(queryParameters, clazz).forEach(new Predicate<>() {
            @Override
            @RequiredReadAction
            public boolean test(JSClass jsClass) {
                JSFunction function = jsClass.findFunctionByNameAndKind(baseFunction.getName(), baseFunction.getKind());
                return function == null || consumer.test(function);
            }
        });
    }

    protected abstract Query<JSClass> makeQuery(SearchParameters queryParameters, PsiElement parent);

    private static class ImplementingFunctionsSearch extends JSFunctionsSearch {
        @Override
        protected Query<JSClass> makeQuery(SearchParameters queryParameters, PsiElement parent) {
            return JSClassSearch.searchInterfaceImplementations((JSClass)parent, true);
        }
    }
}
