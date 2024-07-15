package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class LoopStatementThatDoesntLoopJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.loopStatementThatDoesnTLoopDisplayName().get();
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
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.loopStatementThatDoesntLoopErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSForStatement(@Nonnull JSForStatement statement) {
            super.visitJSForStatement(statement);
            final JSStatement body = statement.getBody();
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
        public void visitJSForInStatement(@Nonnull JSForInStatement statement) {
            super.visitJSForInStatement(statement);
            final JSStatement body = statement.getBody();
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
        public void visitJSWhileStatement(@Nonnull JSWhileStatement statement) {
            super.visitJSWhileStatement(statement);
            final JSStatement body = statement.getBody();
            if (body == null
                || ControlFlowUtils.statementMayCompleteNormally(body)
                || ControlFlowUtils.statementIsContinueTarget(statement)) {
                return;
            }
            registerStatementError(statement);
        }

        @Override public void visitJSDoWhileStatement(@Nonnull JSDoWhileStatement statement) {
            super.visitJSDoWhileStatement(statement);
            final JSStatement body = statement.getBody();
            if (body == null
                || ControlFlowUtils.statementMayCompleteNormally(body)
                || ControlFlowUtils.statementIsContinueTarget(statement)) {
                return;
            }
            registerStatementError(statement);
        }
    }
}