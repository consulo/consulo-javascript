package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSSwitchStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.util.PsiTreeUtil;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class NestedSwitchStatementJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("nested.switch.statement.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("nested.switch.statement.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSSwitchStatement(JSSwitchStatement jsSwitchStatement) {
            super.visitJSSwitchStatement(jsSwitchStatement);
            final JSSwitchStatement containingSwitchStatement =
                    PsiTreeUtil.getParentOfType(jsSwitchStatement, JSSwitchStatement.class, true);
            if (containingSwitchStatement ==null) {
                return;
            }
            registerStatementError(jsSwitchStatement);
        }

    }
}
