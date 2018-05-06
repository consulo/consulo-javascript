package com.sixrr.inspectjs.style;

import com.intellij.lang.javascript.psi.JSArgumentList;
import com.intellij.lang.javascript.psi.JSCallExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import javax.annotation.Nonnull;



public class NestedFunctionCallJSInspection extends JavaScriptInspection {



    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.STYLE_GROUP_NAME;
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "nested.function.call.display.name");
    }

    @Override
	@Nonnull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message(
                "nested.function.call.problem.descriptor");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new NestedMethodCallVisitor();
    }

    private static class NestedMethodCallVisitor extends BaseInspectionVisitor {

        @Override public void visitJSCallExpression(
                @Nonnull JSCallExpression expression) {
            super.visitJSCallExpression(expression);
            JSExpression outerExpression = expression;
            while (outerExpression != null &&
                    outerExpression.getParent()instanceof JSExpression) {
                outerExpression = (JSExpression) outerExpression.getParent();
            }
            if (outerExpression == null) {
                return;
            }
            final PsiElement parent = outerExpression.getParent();
            if (!(parent instanceof JSArgumentList)) {
                return;
            }
            final PsiElement grandParent = parent.getParent();
            if (!(grandParent instanceof JSCallExpression)) {
                return;
            }
            registerFunctionCallError(expression);
        }
    }
}