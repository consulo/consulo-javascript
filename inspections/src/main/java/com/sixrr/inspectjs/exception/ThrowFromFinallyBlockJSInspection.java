package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSThrowStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import org.jetbrains.annotations.NotNull;

public class ThrowFromFinallyBlockJSInspection extends JavaScriptInspection {
    @Override
	@NotNull
    public String getID() {
        return "ThrowInsideFinallyBlockJS";
    }

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("throw.inside.finally.block.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @Override
	public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("throw.inside.finally.block.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSThrowStatement(@NotNull JSThrowStatement statement) {
            super.visitJSThrowStatement(statement);
            if (!ControlFlowUtils.isInFinallyBlock(statement)) {
                return;
            }
            registerStatementError(statement);
        }
    }
}