package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSDoWhileStatement;
import com.intellij.lang.javascript.psi.JSForStatement;
import com.intellij.lang.javascript.psi.JSWhileStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import org.jetbrains.annotations.NotNull;

public class InfiniteLoopJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("infinite.loop.statement.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @Override
	public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("infinite.loop.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {


        @Override public void visitJSForStatement(@NotNull JSForStatement statement) {
            super.visitJSForStatement(statement);
            if (ControlFlowUtils.statementMayCompleteNormally(statement)) {
                return;
            }
            if (ControlFlowUtils.statementContainsReturn(statement)) {
                return;
            }
            registerStatementError(statement);
        }

        @Override public void visitJSWhileStatement(@NotNull JSWhileStatement statement) {

            super.visitJSWhileStatement(statement);
            if (ControlFlowUtils.statementMayCompleteNormally(statement)) {
                return;
            }
            if (ControlFlowUtils.statementContainsReturn(statement)) {
                return;
            }
            registerStatementError(statement);
        }

        @Override public void visitJSDoWhileStatement(@NotNull JSDoWhileStatement statement) {
            super.visitJSDoWhileStatement(statement);
            if (ControlFlowUtils.statementMayCompleteNormally(statement)) {
                return;
            }
            if (ControlFlowUtils.statementContainsReturn(statement)) {
                return;
            }
            registerStatementError(statement);
        }
    }
}