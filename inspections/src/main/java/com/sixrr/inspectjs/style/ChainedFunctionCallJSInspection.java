package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ChainedFunctionCallJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME;
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.chainedFunctionCallDisplayName();
    }

    @Nonnull
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.chainedFunctionCallProblemDescriptor().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new NestedMethodCallVisitor();
    }

    private static class NestedMethodCallVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSCallExpression(@Nonnull JSCallExpression expression) {
            super.visitJSCallExpression(expression);
            final JSExpression reference = expression.getMethodExpression();
            if (!(reference instanceof JSReferenceExpression)) {
                return;
            }
            final JSExpression qualifier = ((JSReferenceExpression)reference).getQualifier();
            if (qualifier == null) {
                return;
            }
            if (!isCallExpression(qualifier)) {
                return;
            }
            registerFunctionCallError(expression);
        }

        private static boolean isCallExpression(JSExpression expression) {
            if (expression instanceof JSCallExpression) {
                return true;
            }
            if (expression instanceof JSParenthesizedExpression parenthesizedExpression) {
                final JSExpression containedExpression = parenthesizedExpression.getInnerExpression();
                return isCallExpression(containedExpression);
            }
            return false;
        }
    }
}
