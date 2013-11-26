package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class IncrementDecrementResultUsedJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("result.of.increment.or.decrement.used.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("result.of.increment.or.decrement.expression.used.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSPrefixExpression(JSPrefixExpression jsPrefixExpression) {
            final IElementType sign = jsPrefixExpression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) && !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            final PsiElement parent = jsPrefixExpression.getParent();
            if (parent == null) {
                return;
            }

            if (parent instanceof JSExpressionStatement
                    || parent instanceof JSForInStatement
                    || parent instanceof JSForStatement
                    || parent instanceof JSCommaExpression) {
                return;
            }
            registerError(jsPrefixExpression);
        }

        @Override public void visitJSPostfixExpression(JSPostfixExpression jsPostfixExpression) {
            final IElementType sign = jsPostfixExpression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) && !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            final PsiElement parent = jsPostfixExpression.getParent();
            if (parent == null) {
                return;
            }
            if (parent instanceof JSExpressionStatement
                    ||parent instanceof JSForInStatement
                    ||parent instanceof JSForStatement
                    || parent instanceof JSCommaExpression) {
                return;
            }
            registerError(jsPostfixExpression);
        }
    }
}
