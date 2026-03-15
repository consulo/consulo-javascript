package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

@ExtensionImpl
public class DebuggerStatementJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.debuggerStatementDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return false;
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.debuggerStatementErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSExpressionStatement(JSExpressionStatement jsExpressionStatement) {
            super.visitJSExpressionStatement(jsExpressionStatement);
            JSExpression expression = jsExpressionStatement.getExpression();

            if (!(expression instanceof JSReferenceExpression)) {
                return;
            }
            String text = expression.getText();
            if (!"debugger".equals(text)) {
                return;
            }
            registerError(expression);
        }
    }
}
