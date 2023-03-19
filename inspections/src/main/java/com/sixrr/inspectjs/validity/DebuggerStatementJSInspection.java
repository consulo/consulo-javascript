package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import org.jetbrains.annotations.NonNls;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class DebuggerStatementJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("debugger.statement.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
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
        return InspectionJSBundle.message("debugger.statement.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSExpressionStatement(JSExpressionStatement jsExpressionStatement) {
            super.visitJSExpressionStatement(jsExpressionStatement);
            final JSExpression expression = jsExpressionStatement.getExpression();

            if (!(expression instanceof JSReferenceExpression)) {
                return;
            }
            @NonNls final String text = expression.getText();
            if (!"debugger".equals(text)) {
                return;
            }
            registerError(expression);
        }
    }
}
