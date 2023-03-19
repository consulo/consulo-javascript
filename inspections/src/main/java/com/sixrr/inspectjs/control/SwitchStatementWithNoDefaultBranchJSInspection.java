package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSCaseClause;
import com.intellij.lang.javascript.psi.JSSwitchStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class SwitchStatementWithNoDefaultBranchJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("switch.statement.with.no.default.branch.display.name");
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
        return InspectionJSBundle.message("switch.statement.with.no.default.branch.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSSwitchStatement(JSSwitchStatement jsSwitchStatement) {
            super.visitJSSwitchStatement(jsSwitchStatement);
            if (hasDefaultBranch(jsSwitchStatement)) {
                return;
            }
            registerStatementError(jsSwitchStatement);
        }

        private static boolean hasDefaultBranch(JSSwitchStatement jsSwitchStatement) {
            final JSCaseClause[] caseClauses = jsSwitchStatement.getCaseClauses();
            for (JSCaseClause clause : caseClauses) {
                if (clause.isDefault()) {
                    return true;
                }
            }
            return false;
        }
    }
}
