package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSCaseClause;
import com.intellij.lang.javascript.psi.JSSwitchStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

@ExtensionImpl
public class SwitchStatementWithNoDefaultBranchJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.switchStatementWithNoDefaultBranchDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.switchStatementWithNoDefaultBranchErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSSwitchStatement(JSSwitchStatement jsSwitchStatement) {
            super.visitJSSwitchStatement(jsSwitchStatement);
            if (hasDefaultBranch(jsSwitchStatement)) {
                return;
            }
            registerStatementError(jsSwitchStatement);
        }

        private static boolean hasDefaultBranch(JSSwitchStatement jsSwitchStatement) {
            JSCaseClause[] caseClauses = jsSwitchStatement.getCaseClauses();
            for (JSCaseClause clause : caseClauses) {
                if (clause.isDefault()) {
                    return true;
                }
            }
            return false;
        }
    }
}
