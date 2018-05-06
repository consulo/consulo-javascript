package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSCaseClause;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSSwitchStatement;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import javax.annotation.Nonnull;

public class TextLabelInSwitchStatementJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("text.label.in.switch.statement.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Override
	@Nonnull
    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("text.label.in.switch.statement.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new TextLabelInSwitchStatementVisitor();
    }

    private static class TextLabelInSwitchStatementVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSSwitchStatement(
                @Nonnull JSSwitchStatement statement) {
            super.visitJSSwitchStatement(statement);
            final JSCaseClause[] caseClauses = statement.getCaseClauses();
            for (JSCaseClause caseClause : caseClauses) {
                final JSStatement[] statements = caseClause.getStatements();
                for (JSStatement statement1 : statements) {
                    checkForLabel(statement1);
                }
            }
        }

        private void checkForLabel(JSStatement statement) {
            if (!(statement instanceof JSLabeledStatement)) {
                return;
            }
            final JSLabeledStatement labeledStatement =
                    (JSLabeledStatement) statement;
            final PsiElement label = labeledStatement.getLabelIdentifier();
            registerError(label);
        }
    }
}
