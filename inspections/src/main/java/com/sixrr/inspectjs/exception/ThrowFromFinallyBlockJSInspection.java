package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSThrowStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ThrowFromFinallyBlockJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getID() {
        return "ThrowInsideFinallyBlockJS";
    }

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.throwInsideFinallyBlockDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME.get();
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.throwInsideFinallyBlockErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSThrowStatement(@Nonnull JSThrowStatement statement) {
            super.visitJSThrowStatement(statement);
            if (!ControlFlowUtils.isInFinallyBlock(statement)) {
                return;
            }
            registerStatementError(statement);
        }
    }
}
