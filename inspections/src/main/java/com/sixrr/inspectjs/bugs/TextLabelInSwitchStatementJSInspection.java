package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSCaseClause;
import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSSwitchStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;

@ExtensionImpl
public class TextLabelInSwitchStatementJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.textLabelInSwitchStatementDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.textLabelInSwitchStatementErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new TextLabelInSwitchStatementVisitor();
    }

    private static class TextLabelInSwitchStatementVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSSwitchStatement(JSSwitchStatement statement) {
            super.visitJSSwitchStatement(statement);
            JSCaseClause[] caseClauses = statement.getCaseClauses();
            for (JSCaseClause caseClause : caseClauses) {
                JSStatement[] statements = caseClause.getStatements();
                for (JSStatement statement1 : statements) {
                    checkForLabel(statement1);
                }
            }
        }

        private void checkForLabel(JSStatement statement) {
            if (!(statement instanceof JSLabeledStatement)) {
                return;
            }
            JSLabeledStatement labeledStatement = (JSLabeledStatement) statement;
            PsiElement label = labeledStatement.getLabelIdentifier();
            registerError(label);
        }
    }
}
