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
import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSVariable;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.psi.xml.XmlFile;
import org.intellij.idea.lang.javascript.intention.JSElementIterator;
import org.intellij.idea.lang.javascript.intention.JSFunctionVisitor;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class FindReferenceUtil {
    private FindReferenceUtil() {}

    public static JSReferenceExpression[] findReferences(@NotNull JSVariable variable) {
        final JSReferenceVisitor visitor = new JSReferenceVisitor(variable, Integer.MAX_VALUE, 0, Integer.MAX_VALUE);
        JSElement                scope   = PsiTreeUtil.getParentOfType(variable, JSFunction.class);

        if (scope == null) {
            scope = getFarthestAncestor(variable, JSElement.class);
        }
        visitor.visitJSElement(scope);

        return visitor.getReferences();
    }

    public static <T extends PsiElement> T getFarthestAncestor(@NotNull PsiElement element, @NotNull Class<T> aClass) {
        PsiElement previousElement = element;
        PsiElement currentElement;

        do {
            currentElement = previousElement.getParent();
            if (!aClass.isInstance(currentElement)) {
                //noinspection unchecked
                return (T) previousElement;
            }
            previousElement = currentElement;
        } while (currentElement != null);

        return null;
    }

    private static final class JSReferenceVisitor extends JSFunctionVisitor {
        private final JSVariable            variable;
        private final String                variableName;
        private final int                   maxCount;
        private final int                   minTextOffset;
        private final int                   maxTextOffset;
        private List<JSReferenceExpression> references;

        public JSReferenceVisitor(@NotNull JSVariable variable, int maxCount, int minTextOffset, int maxTextOffset) {
            this.variable      = variable;
            this.variableName  = variable.getName();
            this.maxCount      = maxCount;
            this.minTextOffset = minTextOffset;
            this.maxTextOffset = maxTextOffset;
            this.references    = new ArrayList<JSReferenceExpression>();
        }

        @Override public void visitElement(PsiElement element) {
            if (this.references.size() < this.maxCount) {
                final int elementTextOffset = element.getTextOffset();

                if (elementTextOffset + element.getTextLength() >= this.minTextOffset &&
                    elementTextOffset <= this.maxTextOffset) {
                    super.visitElement(element);
                }
            }
        }

        @Override public void visitJSReferenceExpression(final JSReferenceExpression expression) {
            super.visitJSReferenceExpression(expression);

            if (expression.getText().equals(this.variableName)) {
                final JSVariable referent = ControlFlowUtils.resolveVariable(expression);

                if (referent != null && this.variable.equals(referent)) {
                    this.references.add(expression);
                }
            }
        }

        public JSReferenceExpression[] getReferences() {
            return this.references.toArray(new JSReferenceExpression[this.references.size()]);
        }

        public JSReferenceExpression getReference(int index) {
            return ((index >= 0 && index < this.references.size())
                        ? this.references.get(index)
                        : null);
        }
    }


    public static Iterable<PsiElement> getReferences(@NotNull JSVariable variable) {
        return getReferences(variable, null, 0, Integer.MAX_VALUE);
    }

    public static Iterable<PsiElement> getReferences(@NotNull JSVariable variable,
                                                     @Nullable PsiElement scope) {
        return getReferences(variable, scope, 0, Integer.MAX_VALUE);
    }

    public static Iterable<PsiElement> getReferencesBefore(@NotNull JSVariable variable, int textOffset) {
        return getReferences(variable, null, 0, textOffset);
    }

    public static Iterable<PsiElement> getReferencesAfter(@NotNull JSVariable variable, int textOffset) {
        return getReferences(variable, null, textOffset, Integer.MAX_VALUE);
    }

    private static Iterable<PsiElement> getReferences(@NotNull  final JSVariable variable,
                                                      @Nullable final PsiElement scope,
                                                      final int minTextOffset, final int maxTextOffset) {
        final PsiElement iteratedScope;

        if (scope == null) {
            JSElement        function = PsiTreeUtil.getParentOfType(variable, JSFunction.class);
            iteratedScope = ((function == null) ? FindReferenceUtil.getFarthestAncestor(variable, XmlFile.class)
                                                : function);
        } else {
            iteratedScope = scope;
        }

        return new Iterable<PsiElement>() {
                @Override
				public Iterator<PsiElement> iterator() {
                    return new JSReferenceIterator(variable, minTextOffset, maxTextOffset, iteratedScope);
                }
            };
    }

    public static final class JSReferenceIterator extends JSElementIterator {
        private final JSVariable            variable;
        private final String                variableName;

        public JSReferenceIterator(@NotNull JSVariable variable,
                                   int minTextOffset, int maxTextOffset,
                                   @NotNull PsiElement element) {
            super(element, false, minTextOffset, maxTextOffset);
            this.variable     = variable;
            this.variableName = variable.getName();
        }

        @Override
		public boolean visitElement(PsiElement element) {
            if (!(element.getText().equals(this.variableName) &&
                  element instanceof JSReferenceExpression)) {
                return false;
            }

            final JSVariable referent = ControlFlowUtils.resolveVariable((JSReferenceExpression) element);

            return (referent != null && this.variable.equals(referent));
        }
    }
}
