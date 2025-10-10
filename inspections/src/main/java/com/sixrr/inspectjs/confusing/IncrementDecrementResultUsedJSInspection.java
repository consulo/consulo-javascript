package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class IncrementDecrementResultUsedJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.resultOfIncrementOrDecrementUsedDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.resultOfIncrementOrDecrementExpressionUsedErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSPrefixExpression(JSPrefixExpression expression) {
            final IElementType sign = expression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) && !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            final PsiElement parent = expression.getParent();
            if (parent == null) {
                return;
            }

            if (parent instanceof JSExpressionStatement
                || parent instanceof JSForInStatement
                || parent instanceof JSForStatement
                || parent instanceof JSCommaExpression) {
                return;
            }
            registerError(expression);
        }

        @Override
        public void visitJSPostfixExpression(JSPostfixExpression jsPostfixExpression) {
            final IElementType sign = jsPostfixExpression.getOperationSign();
            if (!JSTokenTypes.PLUSPLUS.equals(sign) && !JSTokenTypes.MINUSMINUS.equals(sign)) {
                return;
            }
            final PsiElement parent = jsPostfixExpression.getParent();
            if (parent == null) {
                return;
            }
            if (parent instanceof JSExpressionStatement
                || parent instanceof JSForInStatement
                || parent instanceof JSForStatement
                || parent instanceof JSCommaExpression) {
                return;
            }
            registerError(jsPostfixExpression);
        }
    }
}
