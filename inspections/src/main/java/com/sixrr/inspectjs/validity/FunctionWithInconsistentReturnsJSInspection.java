package com.sixrr.inspectjs.validity;

import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.ControlFlowUtils;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.editor.inspection.ProblemHighlightType;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

@ExtensionImpl
public class FunctionWithInconsistentReturnsJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.functionWithInconsistentReturnsDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @Override
    public boolean isEnabledByDefault() {
        return true;
    }

    @Nullable
    @Override
    @RequiredReadAction
    protected String buildErrorString(Object state, Object... args) {
        JSFunction function = (JSFunction)((PsiElement)args[0]).getParent();
        assert function != null;
        return functionHasIdentifier(function)
            ? InspectionJSLocalize.functionHasInconsistentReturnPointsErrorString().get()
            : InspectionJSLocalize.anonymousFunctionHasInconsistentReturnPointsErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
        protected ProblemHighlightType getProblemHighlightType(PsiElement location) {
            return location.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4
                ? ProblemHighlightType.GENERIC_ERROR
                : super.getProblemHighlightType(location);
        }

        @Override
        public void visitJSFunctionDeclaration(JSFunction function) {
            super.visitJSFunctionDeclaration(function);
            String typeString = function.getReturnTypeString();
            if (typeString == null && !functionHasReturnValues(function)) {
                return;
            }
            if ("void".equals(typeString) || !functionHasValuelessReturns(function)) {
                return;
            }
            registerFunctionError(function);
        }

        @Override
        public void visitJSFunctionExpression(JSFunctionExpression node) {
            super.visitJSFunctionExpression(node);
            JSFunction function = node.getFunction();
            String typeString = function.getReturnTypeString();

            if (typeString != null && !"void".equals(typeString)) {
                if (functionHasValuelessReturns(function)) {
                    registerFunctionError(function);
                }
            }
        }
    }

    private static boolean functionHasReturnValues(JSFunction function) {
        ReturnValuesVisitor visitor = new ReturnValuesVisitor(function);
        function.accept(visitor);
        return visitor.hasReturnValues();
    }

    private static boolean functionHasValuelessReturns(JSFunction function) {
        PsiElement lastChild = function.getLastChild();
        if (lastChild instanceof JSBlockStatement) {
            if (ControlFlowUtils.statementMayCompleteNormally((JSStatement)lastChild)) {
                return true;
            }
        }
        ValuelessReturnVisitor visitor = new ValuelessReturnVisitor(function);
        function.acceptChildren(visitor);
        return visitor.hasValuelessReturns();
    }

    private static class ReturnValuesVisitor extends JSRecursiveElementVisitor {
        private final JSFunction function;
        private boolean hasReturnValues = false;

        ReturnValuesVisitor(JSFunction function) {
            this.function = function;
        }

        @Override
        public void visitJSReturnStatement(JSReturnStatement statement) {
            super.visitJSReturnStatement(statement);
            if (statement.getExpression() != null) {
                JSFunction containingFunction = PsiTreeUtil.getParentOfType(statement, JSFunction.class);
                if (function.equals(containingFunction)) {
                    hasReturnValues = true;
                }
            }
        }

        public boolean hasReturnValues() {
            return hasReturnValues;
        }
    }

    private static class ValuelessReturnVisitor extends JSRecursiveElementVisitor {
        private final JSFunction function;
        private boolean hasValuelessReturns = false;

        ValuelessReturnVisitor(JSFunction function) {
            this.function = function;
        }

        @Override
        public void visitJSReturnStatement(JSReturnStatement statement) {
            super.visitJSReturnStatement(statement);
            if (statement.getExpression() == null) {
                JSFunction containingFunction = PsiTreeUtil.getParentOfType(statement, JSFunction.class);
                if (function.equals(containingFunction)) {
                    hasValuelessReturns = true;
                }
            }
        }

        @Override
        public void visitJSFunctionDeclaration(JSFunction function) {
            // do nothing, so that it doesn't drill into nested functions
        }

        public boolean hasValuelessReturns() {
            return hasValuelessReturns;
        }
    }
}
