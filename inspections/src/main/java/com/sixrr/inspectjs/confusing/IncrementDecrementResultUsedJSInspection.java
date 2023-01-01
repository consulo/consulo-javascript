package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.language.ast.IElementType;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class IncrementDecrementResultUsedJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("result.of.increment.or.decrement.used.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("result.of.increment.or.decrement.expression.used.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSPrefixExpression(JSPrefixExpression expression) {
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
