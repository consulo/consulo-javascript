package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.intellij.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class NestedConditionalExpressionJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("nested.conditional.expression.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("nested.conditional.expression.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSConditionalExpression(JSConditionalExpression jsConditionalExpression) {
            super.visitJSConditionalExpression(jsConditionalExpression);
            final JSConditionalExpression containingConditionalExpression =
                    PsiTreeUtil.getParentOfType(jsConditionalExpression, JSConditionalExpression.class, true);
            if (containingConditionalExpression == null) {
                return;
            }
            registerError(jsConditionalExpression);
        }
    }
}
