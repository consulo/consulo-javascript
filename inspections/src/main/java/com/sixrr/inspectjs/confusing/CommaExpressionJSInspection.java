package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.JSCommaExpression;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class CommaExpressionJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("comma.expression.display.name");
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
