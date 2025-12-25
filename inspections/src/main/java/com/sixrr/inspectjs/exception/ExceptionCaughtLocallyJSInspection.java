package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSThrowStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ExceptionCaughtLocallyJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.exceptionUsedForLocalControlFlowDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.exceptionCaughtLocallyErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSThrowStatement(@Nonnull JSThrowStatement statement) {
            super.visitJSThrowStatement(statement);
            if (!isCaughtLocally(statement)) {
                return;
            }
            registerStatementError(statement);
        }
    }

    public static boolean isCaughtLocally(@Nonnull JSThrowStatement throwStatement) {
        JSElement currentElement = throwStatement;
        while (true) {
            JSTryStatement tryStatement = PsiTreeUtil.getParentOfType(currentElement, JSTryStatement.class);
            if (tryStatement == null) {
                return false;
            }
            if (tryStatement.getCatchBlock() != null) {
                JSStatement tryBlock = tryStatement.getStatement();
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