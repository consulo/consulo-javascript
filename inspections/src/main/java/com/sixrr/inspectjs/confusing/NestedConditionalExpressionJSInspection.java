package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSConditionalExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.util.PsiTreeUtil;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class NestedConditionalExpressionJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("nested.conditional.expression.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nullable
    protected String buildErrorString(Object state, Object... args) {
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
