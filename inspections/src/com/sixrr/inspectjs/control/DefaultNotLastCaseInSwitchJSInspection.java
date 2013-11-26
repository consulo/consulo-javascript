package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSCaseClause;
import com.intellij.lang.javascript.psi.JSSwitchStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;

public class DefaultNotLastCaseInSwitchJSInspection extends JavaScriptInspection {

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("default.not.last.case.in.switch.display.name");
    }

    @NotNull
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("default.branch.not.last.case.in.switch.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new DefaultNotLastCaseInSwitchVisitor();
    }

    private static class DefaultNotLastCaseInSwitchVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSSwitchStatement(
                @NotNull JSSwitchStatement statement) {
            super.visitJSSwitchStatement(statement);
            final JSCaseClause[] caseClauses = statement.getCaseClauses();
            if (caseClauses == null) {
                return;
            }
            for (int i = 0; i < caseClauses.length-1; i++) {
                final JSCaseClause caseClause = caseClauses[i];
                if(caseClause.isDefault())
                {
                    registerError(caseClause.getFirstChild());
                }
            }
        }
    }
}