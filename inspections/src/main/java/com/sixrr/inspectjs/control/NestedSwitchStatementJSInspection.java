package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSSwitchStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.util.PsiTreeUtil;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class NestedSwitchStatementJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.nestedSwitchStatementDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.nestedSwitchStatementErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSSwitchStatement(JSSwitchStatement jsSwitchStatement) {
            super.visitJSSwitchStatement(jsSwitchStatement);
            final JSSwitchStatement containingSwitchStatement =
                PsiTreeUtil.getParentOfType(jsSwitchStatement, JSSwitchStatement.class, true);
            if (containingSwitchStatement == null) {
                return;
            }
            registerStatementError(jsSwitchStatement);
        }
    }
}
