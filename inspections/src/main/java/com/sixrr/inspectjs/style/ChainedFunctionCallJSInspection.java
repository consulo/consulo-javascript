package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ChainedFunctionCallJSInspection extends JavaScriptInspection {



    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME;
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "chained.function.call.display.name");
    }

    @RequiredReadAction
	@Override
	@Nonnull
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message(
                "chained.function.call.problem.descriptor");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new NestedMethodCallVisitor();
    }


    private static class NestedMethodCallVisitor extends BaseInspectionVisitor {

        @Override public void visitJSCallExpression(
                @Nonnull JSCallExpression expression) {
            super.visitJSCallExpression(expression);
            final JSExpression reference =
                    expression.getMethodExpression();
            if(!(reference instanceof JSReferenceExpression))
            {
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
            if (expression instanceof JSCallExpression ) {
                return true;
            }
            if (expression instanceof JSParenthesizedExpression) {
                final JSParenthesizedExpression parenthesizedExpression =
                        (JSParenthesizedExpression) expression;
                final JSExpression containedExpression =
                        parenthesizedExpression.getInnerExpression();
                return isCallExpression(containedExpression);
            }
            return false;
        }
    }
}
