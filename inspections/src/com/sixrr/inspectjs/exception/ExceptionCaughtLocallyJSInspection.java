package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSThrowStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.intellij.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;

public class ExceptionCaughtLocallyJSInspection extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("exception.used.for.local.control.flow.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    public boolean isEnabledByDefault() {
        return true;
    }

    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message("exception.caught.locally.error.string");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSThrowStatement(@NotNull JSThrowStatement statement) {
            super.visitJSThrowStatement(statement);
            if (!isCaughtLocally(statement)) {
                return;
            }
            registerStatementError(statement);
        }
    }

    public static boolean isCaughtLocally(@NotNull JSThrowStatement throwStatement) {
        JSElement currentElement = throwStatement;
        while (true) {
            final JSTryStatement tryStatement =
                    PsiTreeUtil.getParentOfType(currentElement,
                            JSTryStatement.class);
            if (tryStatement == null) {
                return false;
            }
            if (tryStatement.getCatchBlock() != null) {
                final JSStatement tryBlock = tryStatement.getStatement();
                if (tryBlock != null) {
                    if (PsiTreeUtil.isAncestor(tryBlock, currentElement, true)) {
                        return true;
                    }
                }
            }
            currentElement = tryStatement;
        }
    }
}