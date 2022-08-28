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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.ast.IElementType;

public class SideEffectChecker {
    private SideEffectChecker() {}

    public static boolean mayHaveSideEffects(JSExpression exp) {
        final SideEffectsVisitor visitor = new SideEffectsVisitor();
        exp.accept(visitor);
        return visitor.mayHaveSideEffects();
    }

    private static class SideEffectsVisitor extends JSRecursiveElementVisitor {
        private boolean mayHaveSideEffects;

        @Override public void visitJSElement(JSElement element) {
            if (!this.mayHaveSideEffects) {
                super.visitJSElement(element);
            }
        }

        @Override public void visitJSCallExpression(JSCallExpression expression) {
            this.mayHaveSideEffects = true;
        }

        @Override public void visitJSNewExpression(JSNewExpression expression) {
            this.mayHaveSideEffects = true;
        }

        @Override public void visitJSAssignmentExpression(JSAssignmentExpression expression) {
            this.mayHaveSideEffects = true;
        }

        @Override public void visitJSPrefixExpression(JSPrefixExpression expression) {
            super.visitJSPrefixExpression(expression);
            final IElementType sign = expression.getOperationSign();

            if (sign != null &&
                (sign.equals(JSTokenTypes.PLUSPLUS)   ||
                 sign.equals(JSTokenTypes.MINUSMINUS))) {
                this.mayHaveSideEffects = true;
            }
        }

        @Override public void visitJSPostfixExpression(JSPostfixExpression expression) {
            super.visitJSPostfixExpression(expression);
            final IElementType sign = expression.getOperationSign();

            if (sign != null &&
                (sign.equals(JSTokenTypes.PLUSPLUS)   ||
                 sign.equals(JSTokenTypes.MINUSMINUS))) {
                this.mayHaveSideEffects = true;
            }
        }

        public boolean mayHaveSideEffects() {
            return this.mayHaveSideEffects;
        }
    }
}
