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

package com.intellij.lang.javascript.impl.validation;

import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.ResolveState;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.Function;

/**
 * @author Maxim.Mossienko
 * @since 2008-07-17
 */
public abstract class ImplementedMethodProcessor extends JSResolveUtil.CollectMethodsToImplementProcessor {
    protected final JSClass myJsClass;

    public ImplementedMethodProcessor(JSClass jsClass) {
        super(null, null);
        myJsClass = jsClass;
    }

    @Override
    protected boolean process(ResolveProcessor processor) {
        Map<String, Object> functions = null;

        for (PsiElement _function : processor.getResults()) {
            if (!(_function instanceof JSFunction function)) {
                continue;
            }
            String name = function.getName();

            if (functions == null) {
                functions = collectAllVisibleClassFunctions(
                    myJsClass,
                    null,
                    jsFunction -> {
                        JSAttributeList attributeList = jsFunction.getAttributeList();
                        PsiElement parentClass = JSResolveUtil.findParent(jsFunction);
                        if ((attributeList == null || attributeList.getAccessType() != JSAttributeList.AccessType.PUBLIC)
                            && myJsClass != parentClass) {
                            return Boolean.FALSE;
                        }
                        return Boolean.TRUE;
                    }
                );
            }

            JSFunction o = findFunctionWithTheSameKind(functions, function, name);

            if (o == null) {
                if (function.isGetProperty() || function.isSetProperty()) {
                    JSVariable var = myJsClass.findFieldByName(name);
                    if (var != null) {
                        JSAttributeList attributeList = var.getAttributeList();
                        if (attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PUBLIC) {
                            continue; // implicit get and set methods
                        }
                    }
                }
                addNonimplementedFunction(function);
            }
            else {
                addImplementedFunction(function, o);
            }
        }
        return true;
    }

    protected void addImplementedFunction(JSFunction interfaceFunction, JSFunction implementationFunction) {
    }

    @RequiredReadAction
    public static JSFunction findFunctionWithTheSameKind(Map<String, Object> functions, JSFunction function, String name) {
        Object o = functions.get(name);
        if (o instanceof JSFunction fun) {
            return fun.getKind() == function.getKind() ? fun : null;
        }
        else if (o instanceof JSFunction[] jsFunctions) {
            for (JSFunction fun : jsFunctions) {
                if (fun.getKind() == function.getKind()) {
                    return fun;
                }
            }
        }
        return null;
    }

    public static Map<String, Object> collectAllVisibleClassFunctions(
        JSClass jsClass,
        Map<String, Object> _functions,
        final @Nullable Function<JSFunction, Boolean> filter
    ) {
        final Map<String, Object> functions = _functions != null ? _functions : new LinkedHashMap<>();
        jsClass.processDeclarations(
            new ResolveProcessor(null) {
                {
                    setToProcessHierarchy(true);
                    setLocalResolve(true);
                }

                @Override
                @RequiredReadAction
                public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                    if (element instanceof JSFunction function) {
                        if (function.isConstructor()) {
                            return true; // SWC stubs have constructor methods :(
                        }

                        JSAttributeList attributeList = function.getAttributeList();
                        if (attributeList != null && attributeList.getAccessType() == JSAttributeList.AccessType.PRIVATE) {
                            return true;
                        }

                        Boolean filterValue = filter != null ? filter.apply(function) : null;
                        if (filterValue != null && !filterValue) {
                            return true;
                        }
                        String s = function.getName();
                        Object function1 = functions.get(s);

                        if (function1 == null) {
                            functions.put(s, function);
                        }
                        else if (function1 instanceof JSFunction function2
                            && findFunctionWithTheSameKind(functions, function, s) == null) {
                            functions.put(s, new JSFunction[]{function2, function});
                        }
                    }
                    return true;
                }
            },
            ResolveState.initial(),
            jsClass,
            jsClass
        );
        return functions;
    }

    protected abstract void addNonimplementedFunction(JSFunction function);
}
