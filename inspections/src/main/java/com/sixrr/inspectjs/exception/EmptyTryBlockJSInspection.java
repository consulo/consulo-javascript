package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.utils.StatementUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class EmptyTryBlockJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("empty.try.block.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    @RequiredReadAction
	@Override
	@Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSBundle.message("empty.try.block.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSTryStatement(JSTryStatement jsTryStatement) {
            super.visitJSTryStatement(jsTryStatement);
            final JSStatement statement = jsTryStatement.getStatement();
            if (statement == null) {
                return;
            }
            if(StatementUtils.isEmpty(statement))
            {
                registerStatementError(jsTryStatement);
            }
        }
    }
}
