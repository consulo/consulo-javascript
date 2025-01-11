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

package com.intellij.lang.javascript.psi;

import jakarta.annotation.Nullable;
import com.intellij.lang.javascript.psi.stubs.JSClassStub;
import consulo.language.psi.StubBasedPsiElement;
import consulo.navigation.Navigatable;

/**
 * @author Maxim.Mossienko
 */
public interface JSClass extends JSQualifiedNamedElement, JSSourceElement, JSAttributeListOwner, Navigatable, StubBasedPsiElement<JSClassStub> {
    JSClass[] EMPTY_ARRAY = new JSClass[0];

    @Nullable
    JSReferenceList getExtendsList();

    @Nullable
    JSReferenceList getImplementsList();

    boolean isInterface();

    JSClass[] getSuperClasses();

    JSFunction[] getFunctions();

    JSVariable[] getFields();

    JSFunction findFunctionByName(final String name);

    JSFunction findFunctionByNameAndKind(final String name, JSFunction.FunctionKind kind);

    JSVariable findFieldByName(final String name);

    JSClass[] getSupers();

    JSClass[] getImplementedInterfaces();

    boolean isDeprecated();
}
