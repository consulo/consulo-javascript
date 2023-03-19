package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSNewExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ObjectAllocationIgnoredJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getID() {
        return "ObjectAllocationIgnored";
    }

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("result.of.object.allocation.ignored.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nonnull
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("result.of.object.allocation.ignored.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new ObjectAllocationIgnoredVisitor();
    }

    private static class ObjectAllocationIgnoredVisitor extends BaseInspectionVisitor {

        @Override public void visitJSExpressionStatement(
                @Nonnull JSExpressionStatement statement) {
            super.visitJSExpressionStatement(statement);
            if (!(statement.getExpression()instanceof JSNewExpression)) {
                return;
            }
            final JSNewExpression newExpression =
                    (JSNewExpression) statement.getExpression();
            final JSExpression methodExpression =
                    newExpression.getMethodExpression();
            if (methodExpression == null) {
                return;
            }
            registerError(methodExpression);
        }
    }
}
