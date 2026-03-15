package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.StatementUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import org.jspecify.annotations.Nullable;

@ExtensionImpl
public class EmptyCatchBlockJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.emptyCatchBlockDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.emptyCatchBlockErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSTryStatement(JSTryStatement jsTryStatement) {
            super.visitJSTryStatement(jsTryStatement);
            JSCatchBlock catchBlock = jsTryStatement.getCatchBlock();
            if (catchBlock == null) {
                return;
            }
            JSStatement body = catchBlock.getStatement();
            if (StatementUtils.isEmpty(body)) {
                PsiElement catchToken = catchBlock.getFirstChild();
                registerError(catchToken);
            }
        }
    }
}
