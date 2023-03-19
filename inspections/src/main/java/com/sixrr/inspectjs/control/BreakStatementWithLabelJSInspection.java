package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSBreakStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class BreakStatementWithLabelJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("break.statement.with.label.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("break.statement.with.label.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSBreakStatement(@Nonnull JSBreakStatement statement) {
            super.visitJSBreakStatement(statement);
            if (statement.getLabel() == null) {
                return;
            }
            registerStatementError(statement);
        }
    }
}
