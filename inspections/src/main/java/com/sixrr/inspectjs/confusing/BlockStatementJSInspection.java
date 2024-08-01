package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class BlockStatementJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.unnecessaryBlockStatementDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    @Nullable
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.unnecessaryBlockStatementErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSBlock(JSBlockStatement jsBlockStatement) {
            super.visitJSBlock(jsBlockStatement);
            final PsiElement parent = jsBlockStatement.getParent();
            if (parent instanceof JSIfStatement
                || parent instanceof JSLoopStatement
                || parent instanceof JSWithStatement
                || parent instanceof JSSwitchStatement
                || parent instanceof JSTryStatement
                || parent instanceof JSCatchBlock
                || parent instanceof JSFunction) {
                return;
            }
            registerStatementError(jsBlockStatement);
        }
    }
}
