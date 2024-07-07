package com.sixrr.inspectjs.utils;

import jakarta.annotation.Nonnull;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.ast.IElementType;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;

public class SideEffectChecker {
    private SideEffectChecker() {
        super();
    }

    public static boolean mayHaveSideEffects(@Nonnull JSExpression exp) {
        final SideEffectsVisitor visitJSor = new SideEffectsVisitor();
        exp.accept(visitJSor);
        return visitJSor.mayHaveSideEffects();
    }

    private static class SideEffectsVisitor extends JSRecursiveElementVisitor {
        private boolean mayHaveSideEffects = false;

        @Override public void visitJSElement(@Nonnull JSElement element) {
            if (!mayHaveSideEffects) {
                super.visitJSElement(element);
            }
        }

        @Override public void visitJSAssignmentExpression(@Nonnull JSAssignmentExpression expression) {
            if (mayHaveSideEffects) {
                return;
            }
            super.visitJSAssignmentExpression(expression);
            mayHaveSideEffects = true;
        }
      
        @Override
		public void visitJSCallExpression(@Nonnull JSCallExpression expression) {
            if (mayHaveSideEffects) {
                return;
            }
            super.visitJSCallExpression(expression);
            mayHaveSideEffects = true;
        }

        @Override public void visitJSNewExpression(@Nonnull JSNewExpression expression) {
            if (mayHaveSideEffects) {
                return;
            }
            super.visitJSNewExpression(expression);
            mayHaveSideEffects = true;
        }

        @Override public void visitJSPostfixExpression(@Nonnull JSPostfixExpression expression) {
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

        @Override public void visitJSPrefixExpression(@Nonnull JSPrefixExpression expression) {
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
