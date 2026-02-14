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

package com.intellij.lang.javascript.psi.impl;

import com.intellij.lang.javascript.JSElementTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.psi.resolve.JSImportHandlingUtil;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import consulo.annotation.access.RequiredReadAction;
import consulo.application.util.CachedValueProvider;
import consulo.application.util.CachedValuesManager;
import consulo.application.util.ParameterizedCachedValue;
import consulo.application.util.UserDataCache;
import consulo.javascript.impl.language.psi.JSStubElementType;
import consulo.language.ast.ASTNode;
import consulo.language.ast.IElementType;
import consulo.language.ast.TokenSet;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiFile;
import consulo.language.psi.PsiModificationTracker;
import consulo.language.psi.resolve.PsiScopeProcessor;
import consulo.language.psi.resolve.ResolveState;
import consulo.util.collection.ArrayFactory;
import consulo.util.collection.ArrayUtil;
import consulo.util.dataholder.Key;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.*;

/**
 * @author Maxim.Mossienko
 */
public abstract class JSClassBase extends JSStubElementImpl<JSClassStub> implements JSClass {
    private static final Key<ParameterizedCachedValue<List<JSClass>, Object>> IMPLEMENTS_LIST_CACHE_KEY = Key.create("implements.list.cache");
    private static final Key<ParameterizedCachedValue<List<JSClass>, Object>> EXTENDS_LIST_CACHE_KEY = Key.create("implements.list.cache");
    private static final UserDataCache<ParameterizedCachedValue<List<JSClass>, Object>, JSClassBase, Object> IMPLEMENTS_LIST_CACHE =
        new ClassesUserDataCache();
    private static final UserDataCache<ParameterizedCachedValue<List<JSClass>, Object>, JSClassBase, Object> EXTENDS_LIST_CACHE =
        new ExtendsClassesUserDataCache();

    private volatile Map<String, Object> myName2FunctionMap;
    private volatile Map<String, JSVariable> myName2FieldsMap;

    protected JSClassBase(ASTNode node) {
        super(node);
    }

    public JSClassBase(JSClassStub stub, JSStubElementType<JSClassStub, JSClass> aClass) {
        super(stub, aClass);
    }

    @Override
    protected void accept(@Nonnull JSElementVisitor visitor) {
        visitor.visitJSClass(this);
    }

    @Override
    public void subtreeChanged() {
        dropCaches();

        super.subtreeChanged();
    }

    @Override
    protected Object clone() {
        JSClassBase o = (JSClassBase)super.clone();
        o.dropCaches();
        return o;
    }

    private void dropCaches() {
        synchronized (this) {
            myName2FunctionMap = null;
            myName2FieldsMap = null;
        }
    }

    @Override
    @RequiredReadAction
    public JSFunction[] getFunctions() {
        JSClassStub classStub = getStub();
        if (classStub != null) {
            return getStubChildrenByType(classStub, JSElementTypes.FUNCTION_DECLARATION, JSFunction.ARRAY_FACTORY);
        }
        else {
            final List<JSFunction> functions = new ArrayList<>();
            processDeclarations(
                new PsiScopeProcessor() {
                    @Override
                    public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                        if (element instanceof JSFunction function) {
                            functions.add(function);
                        }
                        return true;
                    }

                    @Override
                    public <T> T getHint(@Nonnull Key<T> hintClass) {
                        return null;
                    }

                    @Override
                    public void handleEvent(Event event, Object associated) {
                    }
                },
                ResolveState.initial(),
                this,
                this
            );
            return functions.toArray(JSFunction.EMPTY_ARRAY);
        }
    }

    @Override
    @RequiredReadAction
    public JSFunction findFunctionByName(String functionName) {
        if (functionName == null) {
            return null;
        }
        Map<String, Object> name2FunctionMap = initFunctions();
        Object o = name2FunctionMap.get(functionName);
        if (o instanceof JSFunction function) {
            return function;
        }
        else if (o instanceof JSFunction[] functions) {
            return functions[0];
        }
        return null;
    }

    @Override
    @RequiredReadAction
    public JSVariable[] getFields() {
        JSClassStub classStub = getStub();
        final List<JSVariable> vars = new ArrayList<>(3);

        if (classStub != null) {
            for (JSVarStatement var : getStubChildrenByType(classStub, JSElementTypes.VAR_STATEMENT, JSVarStatement[]::new)) {
                vars.addAll(Arrays.asList(var.getVariables()));
            }
        }
        else {
            processDeclarations(
                new PsiScopeProcessor() {
                    @Override
                    public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                        if (element instanceof JSVariable variable) {
                            vars.add(variable);
                        }
                        return true;
                    }

                    @Override
                    public <T> T getHint(@Nonnull Key<T> hintClass) {
                        return null;
                    }

                    @Override
                    public void handleEvent(Event event, Object associated) {
                    }
                },
                ResolveState.initial(),
                this,
                this
            );
        }
        return vars.toArray(JSVariable.EMPTY_ARRAY);
    }

    @Override
    @RequiredReadAction
    public JSVariable findFieldByName(String name) {
        return initFields().get(name);
    }

    @Nonnull
    @RequiredReadAction
    private Map<String, JSVariable> initFields() {
        Map<String, JSVariable> name2FieldsMap = myName2FieldsMap;

        if (name2FieldsMap == null) {
            synchronized (this) {
                name2FieldsMap = myName2FieldsMap;

                if (name2FieldsMap == null) {
                    name2FieldsMap = new HashMap<>();

                    for (JSVariable variable : getFields()) {
                        String name = variable.getName();

                        if (name != null) {
                            name2FieldsMap.put(name, variable);
                        }
                    }
                    myName2FieldsMap = name2FieldsMap;
                }
            }
        }
        return name2FieldsMap;
    }

    @Nonnull
    @RequiredReadAction
    private Map<String, Object> initFunctions() {
        Map<String, Object> name2FunctionMap = myName2FunctionMap;

        if (name2FunctionMap == null) {
            synchronized (this) {
                name2FunctionMap = myName2FunctionMap;

                if (name2FunctionMap == null) {
                    name2FunctionMap = new HashMap<>();

                    for (JSFunction function : getFunctions()) {
                        String name = function.getName();

                        if (name != null) {
                            Object o = name2FunctionMap.get(name);
                            if (o == null) {
                                name2FunctionMap.put(name, function);
                            }
                            else if (o instanceof JSFunction sameNameFunction) {
                                name2FunctionMap.put(name, new JSFunction[]{sameNameFunction, function});
                            }
                            else if (o instanceof JSFunction[] sameNameFunctions) {
                                name2FunctionMap.put(name, ArrayUtil.append(sameNameFunctions, function));
                            }
                        }
                    }
                    myName2FunctionMap = name2FunctionMap;
                }
            }
        }
        return name2FunctionMap;
    }

    @Override
    @RequiredReadAction
    public JSFunction findFunctionByNameAndKind(String name, JSFunction.FunctionKind kind) {
        if (name == null) {
            return null;
        }
        Map<String, Object> name2FunctionMap = initFunctions();
        Object o = name2FunctionMap.get(name);

        if (o instanceof JSFunction function) {
            return function.getKind() == kind ? function : null;
        }
        else if (o instanceof JSFunction[] functions) {
            for (JSFunction fun : functions) {
                if (fun.getKind() == kind) {
                    return fun;
                }
            }
        }
        return null;
    }

    @Override
    public JSClass[] getSupers() {
        List<JSClass> superClasses = new ArrayList<>(getClassesFromReferenceList(getExtendsList(), JSElementTypes.EXTENDS_LIST));
        superClasses.addAll(getClassesFromReferenceList(getImplementsList(), JSElementTypes.IMPLEMENTS_LIST));
        return superClasses.toArray(new JSClass[superClasses.size()]);
    }

    private List<JSClass> getClassesFromReferenceList(@Nullable JSReferenceList extendsList, @Nonnull IElementType type) {
        PsiElement element = extendsList != null ? extendsList : this;

        if (type == JSElementTypes.EXTENDS_LIST) {
            return EXTENDS_LIST_CACHE.get(EXTENDS_LIST_CACHE_KEY, this, extendsList).getValue(element);
        }
        else {
            return IMPLEMENTS_LIST_CACHE.get(IMPLEMENTS_LIST_CACHE_KEY, this, extendsList).getValue(element);
        }
    }

    @Override
    @RequiredReadAction
    public boolean processDeclarations(
        @Nonnull PsiScopeProcessor processor,
        @Nonnull ResolveState substitutor,
        PsiElement lastParent,
        @Nonnull PsiElement place
    ) {
        ResolveProcessor resolveProcessor = processor instanceof ResolveProcessor rProcessor ? rProcessor : null;
        boolean toProcessClass = resolveProcessor != null && resolveProcessor.isTypeContext();

        if (toProcessClass) {
            if (!processor.execute(this, null)) {
                return false;
            }
        }

        if (lastParent == null) {
            return true;
        }

        processor.handleEvent(PsiScopeProcessor.Event.SET_DECLARATION_HOLDER, this);
        boolean toProcessMembers = resolveProcessor == null
            || !resolveProcessor.isToSkipClassDeclarationOnce() && resolveProcessor.isToProcessMembers();
        if (toProcessMembers) {
            if (!processMembers(processor, substitutor, lastParent, place)) {
                return false;
            }
        }
        else {
            resolveProcessor.setToSkipClassDeclarationsOnce(false);
        }

        boolean toProcessInHierarchy = resolveProcessor != null && resolveProcessor.isToProcessHierarchy();

        if (!toProcessInHierarchy || resolveProcessor.checkVisited(getQualifiedName())) {
            return true;
        }

        for (JSClass clazz : getSuperClasses()) {
            if (!clazz.processDeclarations(processor, ResolveState.initial(), lastParent, place)) {
                return false;
            }
        }

        return true;
    }

    protected abstract boolean processMembers(
        PsiScopeProcessor processor,
        ResolveState substitutor,
        PsiElement lastParent,
        PsiElement place
    );

    @Override
    public JSClass[] getSuperClasses() {
        JSReferenceList extendsList = getExtendsList();
        List<JSClass> supers = getClassesFromReferenceList(extendsList, JSElementTypes.EXTENDS_LIST);
        return supers.toArray(new JSClass[supers.size()]);
    }

    @RequiredReadAction
    public static PsiElement findClassFromNamespace(String qName, PsiElement context) {
        return JSResolveUtil.findClassByQName(qName, context);
    }

    @Override
    public JSClass[] getImplementedInterfaces() {
        JSReferenceList implementsList = getImplementsList();
        if (implementsList == null) {
            return JSClass.EMPTY_ARRAY;
        }
        List<JSClass> classes = getClassesFromReferenceList(implementsList, JSElementTypes.IMPLEMENTS_LIST);
        return classes.toArray(new JSClass[classes.size()]);
    }

    private static class ExtendsClassesUserDataCache extends ClassesUserDataCache {
        @Override
        @RequiredReadAction
        protected List<JSClass> doCompute(Object extendsList) {
            if (extendsList instanceof JSClass jsClass) {
                ArrayList<JSClass> supers = new ArrayList<>(1);
                if (!"Object".equals(jsClass.getQualifiedName())) {
                    PsiElement element = findClassFromNamespace("Object", jsClass);
                    if (element instanceof JSClass elementClass) {
                        supers.add(elementClass);
                    }
                }

                return supers;
            }
            return super.doCompute(extendsList);
        }
    }

    private static class ClassesUserDataCache extends UserDataCache<ParameterizedCachedValue<List<JSClass>, Object>, JSClassBase, Object> {
        @Override
        protected ParameterizedCachedValue<List<JSClass>, Object> compute(JSClassBase jsClassBase, Object p) {
            return CachedValuesManager.getManager(jsClassBase.getProject()).createParameterizedCachedValue(
                list -> new CachedValueProvider.Result<>(doCompute(list), PsiModificationTracker.MODIFICATION_COUNT),
                false
            );
        }

        @RequiredReadAction
        protected List<JSClass> doCompute(Object object) {
            if (object instanceof JSClass) {
                return Collections.emptyList();
            }

            ArrayList<JSClass> supers = new ArrayList<>(1);
            JSReferenceList extendsList = (JSReferenceList)object;

            for (String refText : extendsList.getReferenceTexts()) {
                refText = JSImportHandlingUtil.resolveTypeName(refText, extendsList.getParent());
                PsiElement element = findClassFromNamespace(refText, extendsList.getParent());
                if (element instanceof JSClass jsClass) {
                    supers.add(jsClass);
                }
            }
            return supers;
        }
    }

    private static <E extends PsiElement> E[] getStubChildrenByType(JSClassStub stub, IElementType elementType, ArrayFactory<E> f) {
        assert JSElementTypes.INCLUDE_DIRECTIVE != elementType;

        ArrayList<E> result = new ArrayList<>(Arrays.asList(stub.getChildrenByType(elementType, f)));
        JSIncludeDirective[] includes = stub.getChildrenByType(JSElementTypes.INCLUDE_DIRECTIVE, JSIncludeDirective[]::new);
        Collection<JSFile> visited = new HashSet<>();
        TokenSet filter = TokenSet.create(JSElementTypes.INCLUDE_DIRECTIVE, elementType);
        for (JSIncludeDirective include : includes) {
            PsiFile file = include.resolveFile();
            if (file instanceof JSFile jsFile) {
                process(filter, jsFile, result, visited);
            }
        }
        return result.toArray(f.create(result.size()));
    }

    @SuppressWarnings("unchecked")
    private static <E extends PsiElement> void process(
        TokenSet filter,
        JSFile file,
        ArrayList<E> result,
        Collection<JSFile> visited
    ) {
        if (visited.contains(file)) {
            return;
        }
        visited.add(file);
        for (PsiElement element : JSResolveUtil.getStubbedChildren(file, filter)) {
            if (element instanceof JSIncludeDirective includeDirective) {
                PsiFile includedFile = includeDirective.resolveFile();
                if (includedFile instanceof JSFile jsFile) {
                    process(filter, jsFile, result, visited);
                }
            }
            else {
                result.add((E)element);
            }
        }
    }
}
