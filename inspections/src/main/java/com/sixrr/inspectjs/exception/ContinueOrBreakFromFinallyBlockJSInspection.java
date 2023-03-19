package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSBreakStatement;
import com.intellij.lang.javascript.psi.JSContinueStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;

import javax.annotation.Nonnull;

@ExtensionImpl
public class ContinueOrBreakFromFinallyBlockJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("continue.or.break.inside.finally.block.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
	@Override
	public String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("continue.or.break.inside.finally.block.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSContinueStatement(@Nonnull JSContinueStatement statement) {
            super.visitJSContinueStatement(statement);
            if (!ControlFlowUtils.isInFinallyBlock(statement)) {
                return;
            }
            final JSStatement continuedStatement = statement.getStatementToContinue();
            if (continuedStatement == null) {
                return;
            }
            if (ControlFlowUtils.isInFinallyBlock(continuedStatement)) {
                return;
            }
            registerStatementError(statement);
        }

        @Override public void visitJSBreakStatement(@Nonnull JSBreakStatement statement) {
            super.visitJSBreakStatement(statement);
            if (!ControlFlowUtils.isInFinallyBlock(statement)) {
                return;
            }
            final JSStatement exitedStatement = statement.getStatementToBreak();
            if (exitedStatement == null) {
                return;
            }
            if (ControlFlowUtils.isInFinallyBlock(exitedStatement)) {
                return;
            }
            registerStatementError(statement);
        }
    }
}
