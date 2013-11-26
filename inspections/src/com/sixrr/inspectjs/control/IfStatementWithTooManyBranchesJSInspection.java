package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.ui.SingleIntegerFieldOptionsPanel;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class IfStatementWithTooManyBranchesJSInspection extends JavaScriptInspection {
    private static final int DEFAULT_BRANCH_LIMIT = 3;

    /**
     * @noinspection PublicField
     */
    public int m_limit = DEFAULT_BRANCH_LIMIT;  //this is public for the DefaultJDOMExternalizer thingy

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("if.statement.with.too.many.branches.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    private int getLimit() {
        return m_limit;
    }

    public JComponent createOptionsPanel() {
        return new SingleIntegerFieldOptionsPanel(InspectionJSBundle.message("maximum.number.of.branches.parameter"),
                this, "m_limit");
    }

    protected String buildErrorString(Object... args) {
        final JSIfStatement statement = (JSIfStatement) args[0];
        final int branches = calculateNumBranches(statement);
        return InspectionJSBundle.message("if.statement.with.too.many.branches.error.string", branches);
    }

    private static int calculateNumBranches(JSIfStatement statement) {
        final JSStatement branch = statement.getElse();
        if (branch == null) {
            return 1;
        }
        if (!(branch instanceof JSIfStatement)) {
            return 2;
        }
        return 1 + calculateNumBranches((JSIfStatement) branch);
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSIfStatement(@NotNull JSIfStatement statement) {
            super.visitJSIfStatement(statement);
            final PsiElement parent = statement.getParent();
            if (parent instanceof JSIfStatement) {
                final JSIfStatement parentStatement = (JSIfStatement) parent;
                final JSStatement elseBranch = parentStatement.getElse();
                if (statement.equals(elseBranch)) {
                    return;
                }
            }
            final int branches = calculateNumBranches(statement);
            if (branches <= getLimit()) {
                return;
            }
            registerStatementError(statement, statement);
        }
    }
}
