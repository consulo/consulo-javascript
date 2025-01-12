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

import consulo.annotation.access.RequiredReadAction;
import jakarta.annotation.Nullable;

import com.intellij.lang.javascript.psi.stubs.JSAttributeListStub;
import consulo.language.psi.PsiElement;
import consulo.language.psi.StubBasedPsiElement;
import jakarta.annotation.Nonnull;

/**
 * @author Maxim.Mossienko
 */
public interface JSAttributeList extends JSElement, StubBasedPsiElement<JSAttributeListStub> {
    @Nullable
    @RequiredReadAction
    String getNamespace();

    @Nullable
    @RequiredReadAction
    JSReferenceExpression getNamespaceElement();

    JSAttribute[] getAttributes();

    @Nonnull
    @RequiredReadAction
    JSAttribute[] getAttributesByName(@Nonnull String name);

    enum AccessType {
        PACKAGE_LOCAL,
        PUBLIC,
        PRIVATE,
        PROTECTED
    }

    @RequiredReadAction
    AccessType getAccessType();

    @Nullable
    @RequiredReadAction
    PsiElement findAccessTypeElement();

    enum ModifierType {
        DYNAMIC,
        NATIVE,
        OVERRIDE,
        STATIC,
        FINAL,
        VIRTUAL
    }

    @RequiredReadAction
    boolean hasModifier(ModifierType modifier);
}