package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.StatementUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class EmptyTryBlockJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.emptyTryBlockDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.emptyTryBlockErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSTryStatement(JSTryStatement jsTryStatement) {
            super.visitJSTryStatement(jsTryStatement);
            final JSStatement statement = jsTryStatement.getStatement();
            if (statement == null) {
                return;
            }
            if (StatementUtils.isEmpty(statement)) {
                registerStatementError(jsTryStatement);
            }
        }
    }
}
