package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSNewExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;
import org.intellij.lang.annotations.Pattern;

@ExtensionImpl
public class ObjectAllocationIgnoredJSInspection extends JavaScriptInspection {
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "ObjectAllocationIgnored";
    }

    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.resultOfObjectAllocationIgnoredDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.resultOfObjectAllocationIgnoredErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new ObjectAllocationIgnoredVisitor();
    }

    private static class ObjectAllocationIgnoredVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSExpressionStatement(JSExpressionStatement statement) {
            super.visitJSExpressionStatement(statement);
            if (!(statement.getExpression()instanceof JSNewExpression)) {
                return;
            }
            JSNewExpression newExpression = (JSNewExpression) statement.getExpression();
            JSExpression methodExpression = newExpression.getMethodExpression();
            if (methodExpression == null) {
                return;
            }
            registerError(methodExpression);
        }
    }
}
