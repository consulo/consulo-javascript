/*
 * Copyright 2005-2006 Olivier Descout
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
package org.intellij.idea.lang.javascript.psiutil;

import com.intellij.lang.javascript.psi.JSElement;
import consulo.language.psi.PsiElement;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

public class TreeUtil {
    private TreeUtil() {
    }

    public static PsiElement getNextLeaf(PsiElement element) {
        if (element == null) {
            return null;
        }
        PsiElement sibling = element.getNextSibling();
        if (sibling == null) {
            PsiElement parent = element.getParent();
            return getNextLeaf(parent);
        }
        return getFirstLeaf(sibling);
    }

    private static PsiElement getFirstLeaf(PsiElement element) {
        PsiElement[] children = element.getChildren();
        if (children.length == 0) {
            return element;
        }
        return getFirstLeaf(children[0]);
    }

    public static PsiElement getPrevLeaf(PsiElement element) {
        if (element == null) {
            return null;
        }
        PsiElement sibling = element.getPrevSibling();
        if (sibling == null) {
            PsiElement parent = element.getParent();
            return getPrevLeaf(parent);
        }
        return getLastLeaf(sibling);
    }

    private static PsiElement getLastLeaf(PsiElement element) {
        PsiElement[] children = element.getChildren();
        if (children.length == 0) {
            return element;
        }
        return getLastLeaf(children[children.length - 1]);
    }

    @Nullable
    public static <ParentType extends PsiElement> ParentType getParentOfType(
        @Nullable PsiElement element,
        @Nonnull Class<ParentType> aClass
    ) {
        return getParentOfType(element, aClass, true);
    }

    public static <ParentType extends PsiElement> ParentType getParentOfType(
        PsiElement element,
        Class<ParentType> aClass,
        boolean strict
    ) {
        if (element == null) {
            return null;
        }
        if (strict) {
            element = element.getParent();
        }

        while (element != null && !aClass.isInstance(element)) {
            element = element.getParent();
        }

        return (ParentType)element;
    }

    @Nullable
    public static <ParentType extends PsiElement> ParentType getPrevLeafOfType(
        @Nullable PsiElement element,
        @Nonnull Class<ParentType> aClass
    ) {
        return getPrevLeafOfType(element, aClass, true);
    }

    public static <ParentType extends PsiElement> ParentType getPrevLeafOfType(
        PsiElement element,
        Class<ParentType> aClass,
        boolean strict
    ) {
        if (element == null) {
            return null;
        }
        if (strict) {
            element = getPrevLeaf(element);
        }

        while (element != null && !aClass.isInstance(element)) {
            element = getPrevLeaf(element);
        }

        return (ParentType)element;
    }

    @Nullable
    public static <ParentType extends PsiElement> ParentType getNextLeafOfType(
        @Nullable PsiElement element,
        @Nonnull Class<ParentType> aClass
    ) {
        return getNextLeafOfType(element, aClass, true);
    }

    public static <ParentType extends PsiElement> ParentType getNextLeafOfType(
        PsiElement element,
        Class<ParentType> aClass,
        boolean strict
    ) {
        if (element == null) {
            return null;
        }
        if (strict) {
            element = getNextLeaf(element);
        }

        while (element != null && !aClass.isInstance(element)) {
            element = getNextLeaf(element);
        }

        return (ParentType)element;
    }

    public static boolean isAncestor(@Nonnull JSElement ancestor, @Nonnull JSElement element, boolean strict) {
        PsiElement parent = strict ? element.getParent() : element;

        while (parent != null && parent instanceof JSElement) {
            if (parent.equals(ancestor)) {
                return true;
            }

            parent = parent.getParent();
        }

        return false;
    }
}
