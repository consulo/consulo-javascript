package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.InspectionToolState;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class IfStatementWithTooManyBranchesJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.ifStatementWithTooManyBranchesDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Nonnull
    @Override
    public InspectionToolState<?> createStateProvider() {
        return new IfStatementWithTooManyBranchesJSInspectionState();
    }

    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        JSIfStatement statement = (JSIfStatement) args[0];
        int branches = calculateNumBranches(statement);
        return InspectionJSLocalize.ifStatementWithTooManyBranchesErrorString(branches).get();
    }

    private static int calculateNumBranches(JSIfStatement statement) {
        JSStatement branch = statement.getElse();
        if (branch == null) {
            return 1;
        }
        if (!(branch instanceof JSIfStatement)) {
            return 2;
        }
        return 1 + calculateNumBranches((JSIfStatement) branch);
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private class Visitor extends BaseInspectionVisitor<IfStatementWithTooManyBranchesJSInspectionState> {
        @Override
        public void visitJSIfStatement(@Nonnull JSIfStatement statement) {
            super.visitJSIfStatement(statement);
            PsiElement parent = statement.getParent();
            if (parent instanceof JSIfStatement) {
                JSIfStatement parentStatement = (JSIfStatement) parent;
                JSStatement elseBranch = parentStatement.getElse();
                if (statement.equals(elseBranch)) {
                    return;
                }
            }
            int branches = calculateNumBranches(statement);
            if (branches <= myState.m_limit) {
                return;
            }
            registerStatementError(statement, statement);
        }
    }
}
