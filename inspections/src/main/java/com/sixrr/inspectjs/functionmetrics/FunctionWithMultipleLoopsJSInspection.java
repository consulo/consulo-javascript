package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.JSBlockStatement;
import com.intellij.lang.javascript.psi.JSFunction;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class FunctionWithMultipleLoopsJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.functionWithMultipleLoopsDisplayName();
    }

    @Override
    @Nonnull
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME;
    }

    @RequiredReadAction
    @Override
    public String buildErrorString(Object state, Object... args) {
        JSFunction function = (JSFunction)((PsiElement)args[0]).getParent();
        assert function != null;
        LoopCountVisitor visitor = new LoopCountVisitor();
        PsiElement lastChild = function.getLastChild();
        assert lastChild != null;
        lastChild.accept(visitor);
        int loopCount = visitor.getCount();
        return functionHasIdentifier(function)
            ? InspectionJSLocalize.functionContainsMultipleLoopsErrorString(loopCount).get()
            : InspectionJSLocalize.anonymousFunctionContainsMultipleLoopsErrorString(loopCount).get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {
            // note: no call to super
            PsiElement lastChild = function.getLastChild();
            if (!(lastChild instanceof JSBlockStatement)) {
                return;
            }
            LoopCountVisitor visitor = new LoopCountVisitor();
            lastChild.accept(visitor);
            int negationCount = visitor.getCount();
            if (negationCount <= 1) {
                return;
            }
            registerFunctionError(function);
        }
    }
}
