package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;

@ExtensionImpl
public class LoopStatementThatDoesntLoopJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.loopStatementThatDoesnTLoopDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.loopStatementThatDoesntLoopErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSForStatement(JSForStatement statement) {
            super.visitJSForStatement(statement);
            JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (ControlFlowUtils.statementMayCompleteNormally(body)) {
                return;
            }
            if (ControlFlowUtils.statementIsContinueTarget(statement)) {
                return;
            }
            registerStatementError(statement);
        }

        @Override
        public void visitJSForInStatement(JSForInStatement statement) {
            super.visitJSForInStatement(statement);
            JSStatement body = statement.getBody();
            if (body == null) {
                return;
            }
            if (ControlFlowUtils.statementMayCompleteNormally(body)) {
                return;
            }
            if (ControlFlowUtils.statementIsContinueTarget(statement)) {
                return;
            }
            registerStatementError(statement);
        }

        @Override
        public void visitJSWhileStatement(JSWhileStatement statement) {
            super.visitJSWhileStatement(statement);
            JSStatement body = statement.getBody();
            if (body == null
                || ControlFlowUtils.statementMayCompleteNormally(body)
                || ControlFlowUtils.statementIsContinueTarget(statement)) {
                return;
            }
            registerStatementError(statement);
        }

        @Override public void visitJSDoWhileStatement(JSDoWhileStatement statement) {
            super.visitJSDoWhileStatement(statement);
            JSStatement body = statement.getBody();
            if (body == null
                || ControlFlowUtils.statementMayCompleteNormally(body)
                || ControlFlowUtils.statementIsContinueTarget(statement)) {
                return;
            }
            registerStatementError(statement);
        }
    }
}