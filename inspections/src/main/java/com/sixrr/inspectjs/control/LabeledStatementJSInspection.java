package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.JSLabeledStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import org.jetbrains.annotations.NonNls;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class LabeledStatementJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("labeled.statement.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("statement.label.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSLabeledStatement(@Nonnull JSLabeledStatement statement) {
            super.visitJSLabeledStatement(statement);
            @NonNls final String label = statement.getLabel();
            if("javascript".equals(label))
            {
                return;
            }
            registerStatementError(statement);
        }
    }
}
