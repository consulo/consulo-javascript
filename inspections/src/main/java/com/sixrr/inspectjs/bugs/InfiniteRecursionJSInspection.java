package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSFunction;
import com.intellij.openapi.diagnostic.Logger;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.InspectionJSBundle;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import org.jetbrains.annotations.NotNull;

public class InfiniteRecursionJSInspection extends JavaScriptInspection {
    private static Logger logger = Logger.getInstance("ULVJS");

    @Override
	@NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "infinite.recursion.display.name");
    }

    @Override
	@NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Override
	@NotNull
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

        @Override public void visitJSFunctionDeclaration(@NotNull JSFunction function) {
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
