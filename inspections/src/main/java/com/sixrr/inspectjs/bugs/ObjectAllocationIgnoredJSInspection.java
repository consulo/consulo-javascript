package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSNewExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;

public class ObjectAllocationIgnoredJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getID() {
        return "ObjectAllocationIgnored";
    }

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("result.of.object.allocation.ignored.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Override
	@NotNull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("result.of.object.allocation.ignored.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new ObjectAllocationIgnoredVisitor();
    }

    private static class ObjectAllocationIgnoredVisitor extends BaseInspectionVisitor {

        @Override public void visitJSExpressionStatement(
                @NotNull JSExpressionStatement statement) {
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
