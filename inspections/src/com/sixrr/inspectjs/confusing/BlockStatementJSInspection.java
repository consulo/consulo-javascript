package com.sixrr.inspectjs.confusing;

import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class BlockStatementJSInspection extends JavaScriptInspection {

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message("unnecessary.block.statement.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.CONFUSING_GROUP_NAME;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        return InspectionJSBundle.message("unnecessary.block.statement.error.string");
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {

        @Override public void visitJSBlock(JSBlockStatement jsBlockStatement) {
            super.visitJSBlock(jsBlockStatement);
            final PsiElement parent = jsBlockStatement.getParent();
            if(parent instanceof JSIfStatement||
                    parent instanceof JSLoopStatement ||
                    parent instanceof JSWithStatement ||
                    parent instanceof JSSwitchStatement ||
                    parent instanceof JSTryStatement ||
                    parent instanceof JSCatchBlock||
                    parent instanceof JSFunction)
            {
                return;
            }
            registerStatementError(jsBlockStatement);
        }

    }
}
