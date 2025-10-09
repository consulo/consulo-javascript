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
import jakarta.annotation.Nonnull;
import org.intellij.lang.annotations.Pattern;

@ExtensionImpl
public class ObjectAllocationIgnoredJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    @Pattern(value = "[a-zA-Z_0-9.-]+")
    public String getID() {
        return "ObjectAllocationIgnored";
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.resultOfObjectAllocationIgnoredDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Nonnull
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
        public void visitJSExpressionStatement(@Nonnull JSExpressionStatement statement) {
            super.visitJSExpressionStatement(statement);
            if (!(statement.getExpression()instanceof JSNewExpression)) {
                return;
            }
            final JSNewExpression newExpression = (JSNewExpression) statement.getExpression();
            final JSExpression methodExpression = newExpression.getMethodExpression();
            if (methodExpression == null) {
                return;
            }
            registerError(methodExpression);
        }
    }
}
