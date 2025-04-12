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

package com.intellij.lang.javascript.search;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSReferenceList;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.util.function.CommonProcessors;
import consulo.application.util.query.Query;
import consulo.application.util.query.QueryExecutor;
import consulo.application.util.query.QueryFactory;
import consulo.content.scope.SearchScope;
import consulo.javascript.language.psi.stub.JavaScriptIndexKeys;
import consulo.language.psi.scope.GlobalSearchScope;
import consulo.language.psi.stub.StubIndex;
import consulo.language.psi.stub.StubIndexKey;
import consulo.project.Project;
import jakarta.annotation.Nonnull;

import java.util.Collection;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;

public abstract class JSClassSearch implements QueryExecutor<JSClass, JSClassSearch.SearchParameters> {
    public static class SearchParameters {
        private final JSClass myClass;
        private final boolean myCheckDeepInheritance;
        private final GlobalSearchScope myScope;

        public SearchParameters(JSClass anClass, boolean checkDeepInheritance, GlobalSearchScope scope) {
            myClass = anClass;
            myCheckDeepInheritance = checkDeepInheritance;
            myScope = scope;
        }

        public JSClass getTargetClass() {
            return myClass;
        }

        public boolean isCheckDeepInheritance() {
            return myCheckDeepInheritance;
        }

        public GlobalSearchScope getScope() {
            return myScope;
        }
    }

    public static Query<JSClass> searchClassInheritors(JSClass superClass, boolean checkDeepInheritance) {
        SearchParameters parameters = new SearchParameters(superClass, checkDeepInheritance, getUseScope(superClass));
        return CLASS_INHERITORS_QUERY_FACTORY.createUniqueResultsQuery(parameters);
    }

    private static GlobalSearchScope getUseScope(JSClass superClass) {
        SearchScope searchScope = superClass.getUseScope();
        return (GlobalSearchScope)searchScope;
    }

    public static Query<JSClass> searchInterfaceImplementations(JSClass superClass, boolean checkDeepInheritance) {
        SearchParameters parameters = new SearchParameters(superClass, checkDeepInheritance, getUseScope(superClass));
        return INTERFACE_IMPLEMENTATIONS_QUERY_FACTORY.createUniqueResultsQuery(parameters);
    }

    // implementation
    private static final QueryFactory<JSClass, SearchParameters> INTERFACE_IMPLEMENTATIONS_QUERY_FACTORY = new QueryFactory<>();
    private static final QueryFactory<JSClass, SearchParameters> CLASS_INHERITORS_QUERY_FACTORY = new QueryFactory<>();
    private static final JSClassSearch CLASS_SEARCH_EXECUTOR;

    static {
        INTERFACE_IMPLEMENTATIONS_QUERY_FACTORY.registerExecutor(new JSClassSearch() {
            @Override
            protected StubIndexKey<String, JSReferenceList> getIndexKey() {
                return JavaScriptIndexKeys.IMPLEMENTED_INDEX;
            }

            @Override
            protected JSClass[] getSupers(JSClass candidate) {
                return candidate.getImplementedInterfaces();
            }

            @Override
            @RequiredReadAction
            public boolean execute(@Nonnull SearchParameters queryParameters, @Nonnull Predicate<? super JSClass> consumer) {
                Set<JSClass> visited = new HashSet<>();         // no abstract classes in ActionScript !

                if (queryParameters.isCheckDeepInheritance()) {
                    Predicate<? super JSClass> consumerCopy = consumer;
                    consumer = new Predicate<>() {
                        @Override
                        @RequiredReadAction
                        public boolean test(JSClass jsClass) {
                            return consumerCopy.test(jsClass) && CLASS_SEARCH_EXECUTOR.processDirectInheritors(
                                jsClass,
                                this,
                                false,
                                visited,
                                queryParameters.getScope()
                            );
                        }
                    };
                }

                Predicate<? super JSClass> consumerToUse = consumer;
                boolean b = processDirectInheritors(
                    queryParameters.getTargetClass(),
                    consumerToUse,
                    queryParameters.isCheckDeepInheritance(),
                    visited,
                    queryParameters.getScope()
                );
                if (b) {
                    return searchClassInheritors(
                        queryParameters.getTargetClass(),
                        queryParameters.isCheckDeepInheritance()
                    ).forEach((Predicate<? super JSClass>)jsClass -> processDirectInheritors(
                        jsClass,
                        consumerToUse,
                        queryParameters.isCheckDeepInheritance(),
                        visited,
                        queryParameters.getScope()
                    ));
                }
                return b;
            }

            @Override
            protected Collection<JSClass> getInheritors(
                JSClassInheritorsProvider provider,
                String parentName,
                Project project,
                GlobalSearchScope scope
            ) {
                return provider.getImplementingClasses(parentName, project, scope);
            }
        });

        CLASS_INHERITORS_QUERY_FACTORY.registerExecutor(CLASS_SEARCH_EXECUTOR = new JSClassSearch() {
            @Override
            protected StubIndexKey<String, JSReferenceList> getIndexKey() {
                return JavaScriptIndexKeys.EXTENDS_INDEX;
            }

            @Override
            protected JSClass[] getSupers(JSClass candidate) {
                return candidate.getSuperClasses();
            }

            @Override
            protected Collection<JSClass> getInheritors(
                JSClassInheritorsProvider provider,
                String parentName,
                Project project,
                GlobalSearchScope scope
            ) {
                return provider.getExtendingClasses(parentName, project, scope);
            }

        });
    }

    @Override
    @RequiredReadAction
    public boolean execute(@Nonnull SearchParameters queryParameters, @Nonnull Predicate<? super JSClass> consumer) {
        return processDirectInheritors(
            queryParameters.getTargetClass(),
            consumer,
            queryParameters.isCheckDeepInheritance(),
            null,
            queryParameters.getScope()
        );
    }

    @RequiredReadAction
    protected boolean processDirectInheritors(
        JSClass superClass,
        Predicate<? super JSClass> consumer,
        boolean checkDeep,
        Set<JSClass> processed,
        GlobalSearchScope scope
    ) {
        if (processed != null) {
            if (processed.contains(superClass)) {
                return true;
            }
        }
        else {
            processed = new HashSet<>();
        }

        processed.add(superClass);
        Project project = superClass.getProject();
        String name = superClass.getName();
        if (name == null) {
            return true;
        }

        Set<JSClass> temp = processed;
        Predicate<JSClass> processor = candidate -> {
            JSClass[] classes = getSupers(candidate);
            if (classes != null) {
                for (JSClass superClassCandidate : classes) {
                    if (superClassCandidate.isEquivalentTo(superClass)) {
                        if (!consumer.test(candidate)
                            || checkDeep && !processDirectInheritors(candidate, consumer, checkDeep, temp, scope)) {
                            return false;
                        }
                    }
                }
            }

            return true;
        };

        CommonProcessors.CollectProcessor<JSReferenceList> collectProcessor = new CommonProcessors.CollectProcessor<>();

        StubIndex.getInstance().processElements(getIndexKey(), name, project, scope, JSReferenceList.class, collectProcessor);

        for (JSReferenceList referenceList : collectProcessor.getResults()) {
            JSClass parent = (JSClass)referenceList.getParent();
            if (!processor.test(parent)) {
                return false;
            }
        }

        for (JSClassInheritorsProvider provider : JSClassInheritorsProvider.EP_NAME.getExtensionList()) {
            Collection<JSClass> inheritors = getInheritors(provider, name, project, scope);
            for (JSClass inheritor : inheritors) {
                if (!processor.test(inheritor)) {
                    return false;
                }
            }
        }
        return true;
    }

    protected abstract Collection<JSClass> getInheritors(
        JSClassInheritorsProvider provider,
        String parentName,
        Project project,
        GlobalSearchScope scope
    );

    protected abstract StubIndexKey<String, JSReferenceList> getIndexKey();

    protected abstract JSClass[] getSupers(JSClass candidate);
}