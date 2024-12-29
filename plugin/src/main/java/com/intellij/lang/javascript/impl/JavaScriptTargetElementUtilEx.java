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

package com.intellij.lang.javascript.impl;

import com.intellij.lang.javascript.psi.JSClass;
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.resolve.JSResolveUtil;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.TargetElementUtilExtender;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 * @since 2008-10-13
 */
@ExtensionImpl
public class JavaScriptTargetElementUtilEx implements TargetElementUtilExtender {
    @Override
    public boolean includeSelfInGotoImplementation(@Nonnull final PsiElement element) {
        if (element instanceof JSFunction) {
            final PsiElement parent = JSResolveUtil.findParent(element);
            if (parent instanceof JSClass jsClass && jsClass.isInterface()) {
                return false;
            }
        }
        else if (element instanceof JSClass) {
            return false;
        }
        return true;
    }
}
