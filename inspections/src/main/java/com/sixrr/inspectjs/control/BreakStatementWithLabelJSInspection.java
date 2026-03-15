package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSBreakStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

@ExtensionImpl
public class BreakStatementWithLabelJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.breakStatementWithLabelDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.breakStatementWithLabelErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSBreakStatement(JSBreakStatement statement) {
            super.visitJSBreakStatement(statement);
            if (statement.getLabel() == null) {
                return;
            }
            registerStatementError(statement);
        }
    }
}
