package com.sixrr.inspectjs.functionmetrics;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

@ExtensionImpl
public class FunctionWithMultipleReturnPointsJSInspection extends JavaScriptInspection {
    @Override
    public boolean isEnabledByDefault() {
        return false;
    }

    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.functionWithMultipleReturnPointsDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.FUNCTIONMETRICS_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        final JSFunction function = (JSFunction)((PsiElement)args[0]).getParent();
        assert function != null;
        final int returnPointCount = countReturnPoints(function);
        if (functionHasIdentifier(function)) {
            return InspectionJSLocalize.functionContainsMultipleReturnPointsErrorString(returnPointCount).get();
        }
        else {
            return InspectionJSLocalize.anonymousFunctionContainsMultipleReturnPointsErrorString(returnPointCount).get();
        }
    }

    private static int countReturnPoints(JSFunction function) {
        final PsiElement lastChild = function.getLastChild();
        if (!(lastChild instanceof JSBlockStatement)) {
            return 0;
        }
        boolean hasFallthroughReturn = false;
        if (ControlFlowUtils.statementMayCompleteNormally((JSStatement)lastChild)) {
            hasFallthroughReturn = true;
        }
        final ReturnCountVisitor visitor = new ReturnCountVisitor();
        lastChild.accept(visitor);

        final int returnCount = visitor.getReturnCount();
        return hasFallthroughReturn ? returnCount + 1 : returnCount;
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        public void visitJSFunctionDeclaration(@Nonnull JSFunction function) {
            final int returnPointCount = countReturnPoints(function);
            if (returnPointCount <= 1) {
                return;
            }
            registerFunctionError(function);
        }
    }

    private static class ReturnCountVisitor extends JSRecursiveElementVisitor {
        private int returnCount = 0;

        @Override
        public void visitJSElement(JSElement jsElement) {
            int oldCount = 0;
            if (jsElement instanceof JSFunction) {
                oldCount = returnCount;
            }
            super.visitJSElement(jsElement);

            if (jsElement instanceof JSFunction) {
                returnCount = oldCount;
            }
        }

        @Override
        public void visitJSReturnStatement(JSReturnStatement statement) {
            super.visitJSReturnStatement(statement);
            returnCount++;
        }

        public int getReturnCount() {
            return returnCount;
        }
    }
}
