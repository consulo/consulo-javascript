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

/*
 * @author max
 */
package com.intellij.lang.javascript.impl.generation;

import com.intellij.lang.javascript.impl.validation.BaseCreateMethodsFix;
import com.intellij.lang.javascript.impl.validation.ImplementedMethodProcessor;
import com.intellij.lang.javascript.psi.JSAttributeList;
import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.resolve.ResolveProcessor;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.localize.JavaScriptLocalize;
import consulo.language.editor.generation.OverrideMethodHandler;
import consulo.language.psi.PsiElement;
import consulo.language.psi.resolve.ResolveState;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

import java.util.Collection;
import java.util.Map;
import java.util.function.Function;

@ExtensionImpl
public class JavaScriptOverrideMethodsHandler extends BaseJSGenerateHandler implements OverrideMethodHandler {
    @Override
    protected LocalizeValue getTitle() {
        return JavaScriptLocalize.methodsToOverrideChooserTitle();
    }

    @Override
    protected BaseCreateMethodsFix createFix(JSClass clazz) {
        return new OverrideMethodsFix(clazz);
    }

    @Override
    @RequiredReadAction
    protected void collectCandidates(JSClass clazz, final Collection<JSNamedElementNode> candidates) {
        Map<String, Object> _functionsToOverride = null;
        Function<JSFunction, Boolean> functionFilter = function -> {
            JSAttributeList attributeList = function.getAttributeList();

            if (attributeList != null && (attributeList.hasModifier(JSAttributeList.ModifierType.STATIC)
                || attributeList.hasModifier(JSAttributeList.ModifierType.FINAL))) {
                return Boolean.FALSE;
            }
            return Boolean.TRUE;
        };

        for (JSClass superClazz : clazz.getSuperClasses()) {
            _functionsToOverride =
                ImplementedMethodProcessor.collectAllVisibleClassFunctions(superClazz, _functionsToOverride, functionFilter);
        }

        final Map<String, Object> functionsToOverride = _functionsToOverride;
        ResolveProcessor collectOwnFunctions = new ResolveProcessor(null, clazz) {
            {
                setToProcessMembers(true);
                setToProcessHierarchy(false);
            }

            @Override
            @RequiredReadAction
            public boolean execute(@Nonnull PsiElement element, ResolveState state) {
                if (element instanceof JSFunction function) {
                    if (function.isConstructor() || functionsToOverride == null) {
                        return true;
                    }
                    String funName = function.getName();
                    Object o = functionsToOverride.get(funName);
                    if (o instanceof JSFunction oFun && oFun.getKind() == function.getKind()) {
                        functionsToOverride.remove(funName);
                    }
                    else if (o instanceof JSFunction[] oFuns) {
                        functionsToOverride.put(funName, oFuns[0].getKind() == function.getKind() ? oFuns[1] : oFuns[0]);
                    }
                }
                return true;
            }
        };

        clazz.processDeclarations(collectOwnFunctions, ResolveState.initial(), clazz, clazz);

        if (functionsToOverride != null) {
            for (Map.Entry<String, Object> entry : functionsToOverride.entrySet()) {
                Object value = entry.getValue();
                if (value instanceof JSFunction[] functions) {
                    for (JSFunction function : functions) {
                        candidates.add(new JSNamedElementNode(function));
                    }
                }
                else {
                    candidates.add(new JSNamedElementNode((JSFunction)value));
                }
            }
        }
    }
}