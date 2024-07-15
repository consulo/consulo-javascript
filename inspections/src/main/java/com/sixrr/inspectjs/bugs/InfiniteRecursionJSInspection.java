package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSFunction;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.logging.Logger;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class InfiniteRecursionJSInspection extends JavaScriptInspection {
    private static Logger logger = Logger.getInstance("ULVJS");

    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.infiniteRecursionDisplayName().get();
    }

    @Override
    @Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME.get();
    }

    @RequiredReadAction
    @Override
    @Nonnull
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.infiniteRecursionProblemDescriptor().get();
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new InfiniteRecursionVisitor();
    }

    private static class InfiniteRecursionVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {
            super.visitJSFunctionDeclaration(function);

            if (!RecursionUtils.functionMayRecurse(function)
                || !RecursionUtils.functionDefinitelyRecurses(function)) {
                return;
            }
            registerFunctionError(function);
        }
    }
}
