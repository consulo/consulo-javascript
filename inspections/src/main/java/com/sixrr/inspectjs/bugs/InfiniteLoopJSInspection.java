package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSDoWhileStatement;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.intellij.lang.javascript.psi.JSWhileStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class InfiniteLoopJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.infiniteLoopStatementDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME.get();
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.infiniteLoopErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSForStatement(@Nonnull JSForStatement statement) {
            super.visitJSForStatement(statement);
            if (ControlFlowUtils.statementMayCompleteNormally(statement)) {
                return;
            }
            if (ControlFlowUtils.statementContainsReturn(statement)) {
                return;
            }
            registerStatementError(statement);
        }

        @Override
        public void visitJSWhileStatement(@Nonnull JSWhileStatement statement) {
            super.visitJSWhileStatement(statement);
            if (ControlFlowUtils.statementMayCompleteNormally(statement)
                || ControlFlowUtils.statementContainsReturn(statement)) {
                return;
            }
            registerStatementError(statement);
        }

        @Override
        public void visitJSDoWhileStatement(@Nonnull JSDoWhileStatement statement) {
            super.visitJSDoWhileStatement(statement);
            if (ControlFlowUtils.statementMayCompleteNormally(statement)
                || ControlFlowUtils.statementContainsReturn(statement)) {
                return;
            }
            registerStatementError(statement);
        }
    }
}