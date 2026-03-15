package com.sixrr.inspectjs.bugs;

import com.intellij.lang.javascript.psi.JSFunction;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.localize.LocalizeValue;

@ExtensionImpl
public class InfiniteRecursionJSInspection extends JavaScriptInspection {
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.infiniteRecursionDisplayName();
    }

    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
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
        public void visitJSFunctionDeclaration(JSFunction function) {
            super.visitJSFunctionDeclaration(function);

            if (!RecursionUtils.functionMayRecurse(function)
                || !RecursionUtils.functionDefinitelyRecurses(function)) {
                return;
            }
            registerFunctionError(function);
        }
    }
}
