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

package com.intellij.lang.javascript.impl.highlighting;

import com.intellij.lang.javascript.impl.search.JSFunctionsSearch;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.search.JSClassSearch;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.application.progress.ProgressManager;
import consulo.application.util.function.Processor;
import consulo.application.util.query.CollectionQuery;
import consulo.application.util.query.Query;
import consulo.javascript.language.JavaScriptLanguage;
import consulo.language.Language;
import consulo.language.editor.Pass;
import consulo.language.editor.gutter.GutterIconNavigationHandler;
import consulo.language.editor.gutter.LineMarkerInfo;
import consulo.language.editor.gutter.LineMarkerProvider;
import consulo.language.editor.ui.DefaultPsiElementCellRenderer;
import consulo.language.editor.ui.PopupNavigationUtil;
import consulo.language.editor.ui.PsiElementListNavigator;
import consulo.language.psi.NavigatablePsiElement;
import consulo.language.psi.PsiElement;
import consulo.navigation.NavigationItem;
import consulo.platform.base.icon.PlatformIconGroup;
import consulo.ui.annotation.RequiredUIAccess;
import consulo.ui.ex.RelativePoint;
import consulo.util.dataholder.Key;
import consulo.util.lang.ref.SimpleReference;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.awt.event.MouseEvent;
import java.util.*;
import java.util.function.Function;

/**
 * @author Maxim.Mossienko
 * @since 2008-04-14
 */
@ExtensionImpl
public class JavaScriptLineMarkerProvider implements LineMarkerProvider {
    private static final String OVERRIDES_METHOD_IN = "overrides method in ";

    private static final Function<JSClass, String> CLASS_INHERITORS_TOOLTIP_PROVIDER = clazz -> "Has subclasses";

    private static final Function<JSClass, String> IMPLEMENTED_INTERFACES_TOOLTIP_PROVIDER = clazz -> "Has implementations";

    private static final Function<JSFunction, String> OVERRIDDEN_FUNCTIONS_TOOLTIP_PROVIDER = psiElement -> "Is overridden";

    private static final Function<JSFunction, String> IMPLEMENTING_FUNCTIONS_TOOLTIP_PROVIDER = psiElement -> "Is implemented";

    private static final BasicGutterIconNavigationHandler<JSClass> CLASS_INHERITORS_NAV_HANDLER =
        new BasicGutterIconNavigationHandler<>() {
            @Override
            @RequiredReadAction
            protected String getTitle(JSClass elt) {
                return "Choose Subclass of " + elt.getName();
            }

            @Override
            protected Query<JSClass> search(JSClass elt) {
                return JSClassSearch.searchClassInheritors(elt, true);
            }
        };

    private static final BasicGutterIconNavigationHandler<JSClass> INTERFACE_IMPLEMENTATIONS_NAV_HANDLER =
        new BasicGutterIconNavigationHandler<>() {
            @Override
            @RequiredReadAction
            protected String getTitle(JSClass elt) {
                return "Choose Implementation of " + elt.getName();
            }

            @Override
            protected Query<JSClass> search(JSClass elt) {
                return JSClassSearch.searchInterfaceImplementations(elt, true);
            }
        };

    private static final BasicGutterIconNavigationHandler<JSFunction> OVERRIDDEN_FUNCTIONS_NAV_HANDLER =
        new BasicGutterIconNavigationHandler<>() {
            @Override
            @RequiredReadAction
            protected String getTitle(JSFunction elt) {
                return "Choose Overriden Function of " + elt.getName();
            }

            @Override
            @RequiredReadAction
            protected Query<JSFunction> search(JSFunction elt) {
                return doFindOverridenFunctionStatic(elt);
            }
        };

    @RequiredReadAction
    public static Query<JSFunction> doFindOverridenFunctionStatic(JSFunction elt) {
        PsiElement parent = JSResolveUtil.findParent(elt);
        if (parent instanceof JSClass) {
            return JSFunctionsSearch.searchOverridingFunctions(elt, true);
        }
        String qName = JSResolveUtil.getQNameToStartHierarchySearch(elt);
        if (qName != null) {
            ArrayList<JSFunction> result = new ArrayList<>();

            return new CollectionQuery<>(result);
        }

        return new CollectionQuery<>(Collections.<JSFunction>emptyList());
    }

    private static final BasicGutterIconNavigationHandler<JSFunction> IMPLEMENTING_FUNCTIONS_NAV_HANDLER =
        new BasicGutterIconNavigationHandler<>() {
            @Override
            @RequiredReadAction
            protected String getTitle(JSFunction elt) {
                return "Choose Implementation of " + elt.getName();
            }

            @Override
            protected Query<JSFunction> search(JSFunction elt) {
                return JSFunctionsSearch.searchImplementingFunctions(elt, true);
            }
        };

    public static Key<Boolean> ourParticipatesInHierarchyKey = Key.create("js.named.item.participates.in.hierarchy");

    @Override
    @RequiredReadAction
    public LineMarkerInfo getLineMarkerInfo(@Nonnull final PsiElement element) {
        if (element instanceof JSFunction function) {
            function.putUserData(ourParticipatesInHierarchyKey, null);
            if (function.getNameIdentifier() == null) {
                return null;
            }
            final String qName = JSResolveUtil.getQNameToStartHierarchySearch(function);

            if (qName != null) {
                PsiElement parentNode = element.getParent();
                if (parentNode instanceof JSFile jsFile) {
                    JSClass xmlBackedClass = JSResolveUtil.getXmlBackedClass(jsFile);
                    if (xmlBackedClass != null) {
                        parentNode = xmlBackedClass;
                    }
                }

                if (element instanceof JSFunctionExpression functionExpr) {
                    parentNode = functionExpr.getContainingFile();
                }

                final MyOverrideHandler overrideHandler = new MyOverrideHandler();
                final String typeName = parentNode instanceof JSClass jsClass ? jsClass.getQualifiedName() : qName;
                JSResolveUtil.iterateType(function, parentNode, typeName, overrideHandler);

                if (overrideHandler.className != null) {
                    final PsiElement parentNode1 = parentNode;
                    function.putUserData(ourParticipatesInHierarchyKey, Boolean.TRUE);

                    return new LineMarkerInfo<>(
                        function,
                        function.getNameIdentifier().getTextRange().getStartOffset(),
                        PlatformIconGroup.gutterOverridingmethod(),
                        Pass.UPDATE_ALL,
                        psiElement -> OVERRIDES_METHOD_IN + overrideHandler.className,
                        (e, elt) -> {
                            final Set<NavigationItem> results = new HashSet<>();
                            JSResolveUtil.iterateType(
                                function,
                                parentNode1,
                                typeName,
                                (processor, scope, className) -> {
                                    for (PsiElement e1 : processor.getResults()) {
                                        results.add((NavigationItem)e1);
                                    }
                                    return true;
                                }
                            );

                            if (results.size() == 1) {
                                results.iterator().next().navigate(true);
                            }
                            else if (results.size() > 1) {
                                PopupNavigationUtil.getPsiElementPopup(
                                    results.toArray(new PsiElement[results.size()]),
                                    "Choose super class or interface"
                                ).show(new RelativePoint(e));
                            }
                        }
                    );
                }
            }
        }

        return null;
    }

    @Override
    @RequiredReadAction
    public void collectSlowLineMarkers(@Nonnull final List<PsiElement> elements, @Nonnull final Collection<LineMarkerInfo> result) {
        final Map<String, Set<JSFunction>> jsFunctionsToProcess = new HashMap<>();
        final Map<JSClass, Set<JSFunction>> jsMethodsToProcess = new HashMap<>();

        for (final PsiElement el : elements) {
            ProgressManager.getInstance().checkCanceled();

            if (el instanceof JSFunction function) {
                if (isNotApplicableForOverride(function)) {
                    continue;
                }

                PsiElement parent = function.getParent();
                if (parent instanceof JSFile jsFile) {
                    parent = JSResolveUtil.getClassReferenceForXmlFromContext(jsFile);
                }

                if (parent instanceof JSClass jsClass) {
                    Set<JSFunction> functions = jsMethodsToProcess.get(jsClass);
                    if (functions == null) {
                        functions = new HashSet<>();
                        jsMethodsToProcess.put(jsClass, functions);
                    }

                    functions.add(function);
                }
                else if (parent instanceof JSFile || function instanceof JSFunctionExpression) {
                    String qName = JSResolveUtil.getQNameToStartHierarchySearch(function);
                    if (qName != null) {
                        Set<JSFunction> functions = jsFunctionsToProcess.get(qName);

                        if (functions == null) {
                            functions = new HashSet<>();
                            jsFunctionsToProcess.put(qName, functions);
                        }

                        functions.add(function);
                    }
                }
            }
            else if (el instanceof JSClass jsClass) {
                if (!jsMethodsToProcess.containsKey(jsClass)) {
                    jsMethodsToProcess.put(jsClass, null);
                }
            }
        }

        for (Map.Entry<JSClass, Set<JSFunction>> entry : jsMethodsToProcess.entrySet()) {
            ProgressManager.getInstance().checkCanceled();
            final JSClass clazz = entry.getKey();
            final Set<JSFunction> methods = entry.getValue();

            Query<JSClass> classQuery = JSClassSearch.searchClassInheritors(clazz, methods != null);

            classQuery.forEach(new Processor<>() {
                boolean addedClassMarker;
                final Set<JSFunction> methodsClone = methods == null || clazz.isInterface() ? null : new HashSet<>(methods);

                @Override
                public boolean process(JSClass jsClass) {
                    if (!addedClassMarker) {
                        result.add(new LineMarkerInfo<>(
                            clazz,
                            clazz.getTextOffset(),
                            PlatformIconGroup.gutterOverridenmethod(),
                            Pass.LINE_MARKERS,
                            CLASS_INHERITORS_TOOLTIP_PROVIDER,
                            CLASS_INHERITORS_NAV_HANDLER
                        ));
                        addedClassMarker = true;
                    }

                    if (methodsClone != null) {
                        for (final Iterator<JSFunction> functionIterator = methodsClone.iterator(); functionIterator.hasNext(); ) {
                            JSFunction function = functionIterator.next();

                            JSFunction byName = jsClass.findFunctionByNameAndKind(function.getName(), function.getKind());
                            if (byName != null && !isNotApplicableForOverride(byName)) {
                                // TODO: more correct check for override
                                function.putUserData(ourParticipatesInHierarchyKey, Boolean.TRUE);
                                result.add(new LineMarkerInfo<>(
                                    function,
                                    function.getTextOffset(),
                                    PlatformIconGroup.gutterOverridenmethod(),
                                    Pass.LINE_MARKERS,
                                    OVERRIDDEN_FUNCTIONS_TOOLTIP_PROVIDER,
                                    OVERRIDDEN_FUNCTIONS_NAV_HANDLER
                                ));
                                functionIterator.remove();
                            }
                        }
                    }
                    return methodsClone != null && !methodsClone.isEmpty();
                }
            });


            if (clazz.isInterface()) {
                classQuery = JSClassSearch.searchInterfaceImplementations(clazz, false);

                if (classQuery.findFirst() != null) {
                    result.add(new LineMarkerInfo<>(
                        clazz,
                        clazz.getTextOffset(),
                        PlatformIconGroup.gutterImplementedmethod(),
                        Pass.LINE_MARKERS,
                        IMPLEMENTED_INTERFACES_TOOLTIP_PROVIDER,
                        INTERFACE_IMPLEMENTATIONS_NAV_HANDLER
                    ));
                }
            }

            if (methods == null) {
                continue;
            }

            for (JSFunction function : methods) {
                if (clazz.isInterface()) {
                    Query<JSFunction> query = JSFunctionsSearch.searchImplementingFunctions(function, false);
                    if (query.findFirst() != null) {
                        function.putUserData(ourParticipatesInHierarchyKey, Boolean.TRUE);
                        result.add(new LineMarkerInfo<>(
                            function,
                            function.getTextOffset(),
                            PlatformIconGroup.gutterImplementedmethod(),
                            Pass.LINE_MARKERS,
                            IMPLEMENTING_FUNCTIONS_TOOLTIP_PROVIDER,
                            IMPLEMENTING_FUNCTIONS_NAV_HANDLER
                        ));
                    }
                }
                else {
                    JSAttributeList attributeList = function.getAttributeList();
                    if (attributeList != null && attributeList.hasModifier(JSAttributeList.ModifierType.OVERRIDE)) {
                        continue;
                    }

                    JSFunction implementedFunction = findImplementedFunction(function);
                    if (implementedFunction != null) {
                        function.putUserData(ourParticipatesInHierarchyKey, Boolean.TRUE);

                        result.add(new LineMarkerInfo<>(
                            function,
                            function.getTextOffset(),
                            PlatformIconGroup.gutterImplementingmethod(),
                            Pass.LINE_MARKERS,
                            jsFunction -> "Implementation of " + jsFunction.getName() + " in " +
                                ((NavigationItem)implementedFunction.getParent()).getName(),
                            (e, elt) -> {
                                JSFunction implementedFunction1 = findImplementedFunction(elt);
                                if (implementedFunction1 != null) {
                                    implementedFunction1.navigate(true);
                                }
                            }
                        ));
                    }
                }
            }
        }
    }

    private static boolean isClass(PsiElement element) {
        return element instanceof JSClass || element instanceof JSFile jsFile && jsFile.getContext() != null;
    }

    @Nullable
    @RequiredReadAction
    private static JSFunction findImplementedFunction(JSFunction implementingFunction) {
        PsiElement clazz = implementingFunction.getParent();
        if (!(clazz instanceof JSClass)) {
            clazz = JSResolveUtil.getClassReferenceForXmlFromContext(clazz);
        }
        if (!(clazz instanceof JSClass)) {
            return null;
        }
        final SimpleReference<JSFunction> result = new SimpleReference<>();
        JSResolveUtil.processInterfaceMethods(
            (JSClass)clazz,
            new JSResolveUtil.CollectMethodsToImplementProcessor(
                implementingFunction.getName(),
                implementingFunction
            ) {
                @Override
                protected boolean process(ResolveProcessor processor) {
                    result.set((JSFunction)processor.getResult());
                    return false;
                }
            }
        );
        return result.get();
    }

    @RequiredReadAction
    private static boolean isNotApplicableForOverride(JSFunction function) {
        JSAttributeList attributeList = function.getAttributeList();

        return function.isConstructor() || (attributeList != null
            && (attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE
            || attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)
            || attributeList.hasModifier(JSAttributeList.ModifierType.NATIVE)));
    }

    static class MyOverrideHandler implements JSResolveUtil.OverrideHandler {
        String className;

        @Override
        public boolean process(ResolveProcessor processor, PsiElement scope, String className) {
            this.className = className;
            return true;
        }
    }

    private abstract static class BasicGutterIconNavigationHandler<T extends PsiElement> implements GutterIconNavigationHandler<T> {
        @Override
        @RequiredUIAccess
        public void navigate(MouseEvent e, T elt) {
            final List<NavigatablePsiElement> navElements = new ArrayList<>();
            Query<T> elementQuery = search(elt);
            if (elementQuery == null) {
                return;
            }
            elementQuery.forEach(psiElement -> {
                if (psiElement instanceof NavigatablePsiElement navigatablePsiElement) {
                    navElements.add(navigatablePsiElement);
                }
                return true;
            });
            NavigatablePsiElement[] methods = navElements.toArray(new NavigatablePsiElement[navElements.size()]);
            PsiElementListNavigator.openTargets(e, methods, getTitle(elt), "", new DefaultPsiElementCellRenderer());
        }

        protected abstract String getTitle(T elt);

        @Nullable
        protected abstract Query<T> search(T elt);
    }

    @Nonnull
    @Override
    public Language getLanguage() {
        return JavaScriptLanguage.INSTANCE;
    }
}
