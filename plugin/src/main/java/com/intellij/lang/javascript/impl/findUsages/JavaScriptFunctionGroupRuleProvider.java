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

package com.intellij.lang.javascript.impl.findUsages;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSNamedElement;
import consulo.annotation.component.ExtensionImpl;
import consulo.application.AllIcons;
import consulo.usage.UsageGroup;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 */
@ExtensionImpl
public class JavaScriptFunctionGroupRuleProvider extends JavaScriptGroupRuleProviderBase<JSFunction> {
    @Override
    protected Class<? extends JSNamedElement> getUsageClass() {
        return JSFunction.class;
    }

    @Override
    protected UsageGroup createUsageGroup(final JSFunction jsFunction) {
        return new FunctionUsageGroup(jsFunction);
    }

    private static class FunctionUsageGroup extends JavaScriptGroupRuleProviderBase.PsiNamedElementUsageGroupBase<JSFunction> {
        public FunctionUsageGroup(@Nonnull JSFunction function) {
            super(function, AllIcons.Nodes.Function);
        }
    }
}
