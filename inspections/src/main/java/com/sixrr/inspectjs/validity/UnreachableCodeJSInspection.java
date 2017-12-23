package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class UnreachableCodeJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("unreachable.code.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("unreachable.code.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        
        //TODO: this doesn't work at the top level
        @Override public void visitJSBlock(JSBlockStatement statement) {
            super.visitJSBlock(statement);
            final JSStatement[] statements = statement.getStatements();
            for (int i = 0; i < statements.length-1; i++) {
                if(!ControlFlowUtils.statementMayCompleteNormally(statements[i]))
                {
                    registerStatementError(statements[i+1]);
                }

            }
        }
    }
}
