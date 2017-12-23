package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSCommaExpression;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class CommaExpressionJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("comma.expression.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("comma.expression.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSCommaExpression(JSCommaExpression node) {
            super.visitJSCommaExpression(node);
            if(node.getParent() instanceof JSCommaExpression)
            {
                return;
            }
            if(node.getParent() instanceof JSForStatement)
            {
                return;
            }
            registerError(node);
        }

    }
}
