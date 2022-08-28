package com.sixrr.inspectjs.exception;

import com.intellij.lang.javascript.psi.JSCatchBlock;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.lang.javascript.psi.JSTryStatement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.utils.StatementUtils;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

@ExtensionImpl
public class EmptyCatchBlockJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("empty.catch.block.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.ERRORHANDLING_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("empty.catch.block.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override public void visitJSTryStatement(JSTryStatement jsTryStatement) {
            super.visitJSTryStatement(jsTryStatement);
            final JSCatchBlock catchBlock = jsTryStatement.getCatchBlock();
            if (catchBlock == null) {
                return;
            }
            final JSStatement body = catchBlock.getStatement();
            if(StatementUtils.isEmpty(body))
            {
                final PsiElement catchToken = catchBlock.getFirstChild();
                registerError(catchToken);
            }
        }
    }
}
