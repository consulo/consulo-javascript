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

@ExtensionImpl
public class DefaultNotLastCaseInSwitchJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.defaultNotLastCaseInSwitchDisplayName();
    }

    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.defaultBranchNotLastCaseInSwitchErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new DefaultNotLastCaseInSwitchVisitor();
    }

    private static class DefaultNotLastCaseInSwitchVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSSwitchStatement(JSSwitchStatement statement) {
            super.visitJSSwitchStatement(statement);
            JSCaseClause[] caseClauses = statement.getCaseClauses();
            if (caseClauses == null) {
                return;
            }
            for (int i = 0; i < caseClauses.length - 1; i++) {
                JSCaseClause caseClause = caseClauses[i];
                if (caseClause.isDefault()) {
                    registerError(caseClause.getFirstChild());
                }
            }
        }
    }
}