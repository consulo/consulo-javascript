package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSCaseClause;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSSwitchStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class FallthroughInSwitchStatementJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.fallThroughInSwitchStatementDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME.get();
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.fallThroughInSwitchStatementErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSSwitchStatement(JSSwitchStatement jsSwitchStatement) {
            final JSCaseClause[] caseClauses = jsSwitchStatement.getCaseClauses();
            for (int i = 0; i < caseClauses.length-1; i++) {
                final JSCaseClause clause = caseClauses[i];
                final JSStatement[] statements = clause.getStatements();
                if (statements != null && statements.length != 0) {
                    final JSStatement lastStatementOfClause = statements[statements.length - 1];
                    if (ControlFlowUtils.statementMayCompleteNormally(lastStatementOfClause)) {
                        final PsiElement caseIdentifier = caseClauses[i+1].getFirstChild();
                        registerError(caseIdentifier);
                    }
                }
            }
        }
    }
}
