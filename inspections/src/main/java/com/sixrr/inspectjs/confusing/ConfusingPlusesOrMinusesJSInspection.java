package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSPostfixExpression;
import com.intellij.lang.javascript.psi.JSPrefixExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class ConfusingPlusesOrMinusesJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.confusingPlusesOrMinusesDisplayName();
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
        return InspectionJSLocalize.confusingPlusesOrMinusesErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSBinaryExpression(JSBinaryExpression jsBinaryExpression) {
            super.visitJSBinaryExpression(jsBinaryExpression);
            IElementType sign = jsBinaryExpression.getOperationSign();
            if (!JSTokenTypes.PLUS.equals(sign) && !JSTokenTypes.MINUS.equals(sign)) {
                return;
            }
            JSExpression rhs = jsBinaryExpression.getROperand();
            if (rhs == null) {
                return;
            }
            JSExpression lhs = jsBinaryExpression.getLOperand();
            if (lhs == null) {
                return;
            }
            if (lhs instanceof JSPostfixExpression) {
                JSPostfixExpression postfixExpression = (JSPostfixExpression)lhs;
                IElementType innerSign = postfixExpression.getOperationSign();
                if (matches(sign, innerSign)) {
                    registerError(jsBinaryExpression);
                    return;
                }
            }
            if (rhs instanceof JSPrefixExpression) {
                JSPrefixExpression postfixExpression = (JSPrefixExpression)rhs;
                IElementType innerSign = postfixExpression.getOperationSign();
                if (matches(sign, innerSign)) {
                    registerError(jsBinaryExpression);
                }
            }
        }

        private static boolean matches(IElementType sign, IElementType innerSign) {
            if (JSTokenTypes.PLUS.equals(sign)) {
                return JSTokenTypes.PLUSPLUS.equals(innerSign);
            }
            else {
                return JSTokenTypes.MINUSMINUS.equals(innerSign);
            }
        }
    }
}
