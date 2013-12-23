package com.sixrr.inspectjs.utils;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.tree.IElementType;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import org.jetbrains.annotations.NotNull;

public class SideEffectChecker {
    private SideEffectChecker() {
        super();
    }

    public static boolean mayHaveSideEffects(@NotNull JSExpression exp) {
        final SideEffectsVisitor visitJSor = new SideEffectsVisitor();
        exp.accept(visitJSor);
        return visitJSor.mayHaveSideEffects();
    }

    private static class SideEffectsVisitor extends JSRecursiveElementVisitor {
        private boolean mayHaveSideEffects = false;

        @Override public void visitJSElement(@NotNull JSElement element) {
            if (!mayHaveSideEffects) {
                super.visitJSElement(element);
            }
        }

        @Override public void visitJSAssignmentExpression(@NotNull JSAssignmentExpression expression) {
            if (mayHaveSideEffects) {
                return;
            }
            super.visitJSAssignmentExpression(expression);
            mayHaveSideEffects = true;
        }
      
        @Override
		public void visitJSCallExpression(@NotNull JSCallExpression expression) {
            if (mayHaveSideEffects) {
                return;
            }
            super.visitJSCallExpression(expression);
            mayHaveSideEffects = true;
        }

        @Override public void visitJSNewExpression(@NotNull JSNewExpression expression) {
            if (mayHaveSideEffects) {
                return;
            }
            super.visitJSNewExpression(expression);
            mayHaveSideEffects = true;
        }

        @Override public void visitJSPostfixExpression(@NotNull JSPostfixExpression expression) {
            if (mayHaveSideEffects) {
                return;
            }
            super.visitJSPostfixExpression(expression);
            final IElementType sign = expression.getOperationSign();
            if (JSTokenTypes.PLUSPLUS.equals(sign) ||
                    JSTokenTypes.MINUSMINUS.equals(sign)) {
                mayHaveSideEffects = true;
            }
        }

        @Override public void visitJSPrefixExpression(@NotNull JSPrefixExpression expression) {
            if (mayHaveSideEffects) {
                return;
            }
            super.visitJSPrefixExpression(expression);
            final IElementType sign = expression.getOperationSign();
            if (JSTokenTypes.PLUSPLUS.equals(sign) ||
                    JSTokenTypes.MINUSMINUS.equals(sign)) {
                mayHaveSideEffects = true;
            }
        }

        private boolean mayHaveSideEffects() {
            return mayHaveSideEffects;
        }
    }
}
