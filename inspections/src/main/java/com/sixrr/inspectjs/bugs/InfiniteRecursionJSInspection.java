package com.sixrr.inspectjs.bugs;

import javax.annotation.Nonnull;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.openapi.diagnostic.Logger;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;

public class InfiniteRecursionJSInspection extends JavaScriptInspection {
    private static Logger logger = Logger.getInstance("ULVJS");

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "infinite.recursion.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Override
	@Nonnull
    public String buildErrorString(Object... args) {
        return InspectionJSBundle.message(
                "infinite.recursion.problem.descriptor");
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new InfiniteRecursionVisitor();
    }

    private static class InfiniteRecursionVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {
            super.visitJSFunctionDeclaration(function);

            if (!RecursionUtils.functionMayRecurse(function)) {
                return;
            }
            if (!RecursionUtils.functionDefinitelyRecurses(function)) {
                return;
            }
            registerFunctionError(function);
        }
    }
}
