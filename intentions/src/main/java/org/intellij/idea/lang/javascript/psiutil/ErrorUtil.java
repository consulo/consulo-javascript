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

import consulo.language.editor.util.PsiUtilBase;
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiErrorElement;
import consulo.language.psi.PsiRecursiveElementVisitor;
import jakarta.annotation.Nonnull;

public class ErrorUtil {
    private ErrorUtil() {
    }

    private static final boolean fullTraversal = false;

    public static boolean containsError(@Nonnull PsiElement element) {
        // check only immediate children, full tree traversal is too expensive
        if (fullTraversal) {
            ErrorElementVisitor visitor = new ErrorElementVisitor();

            element.accept(visitor);
            return visitor.containsErrorElement();
        }
        else {
            return PsiUtilBase.hasErrorElementChild(element);
        }
    }

    private static class ErrorElementVisitor extends PsiRecursiveElementVisitor {
        private boolean containsErrorElement;

        @Override
        public void visitErrorElement(PsiErrorElement element) {
            this.containsErrorElement = true;
        }

        private boolean containsErrorElement() {
            return this.containsErrorElement;
        }
    }
}
