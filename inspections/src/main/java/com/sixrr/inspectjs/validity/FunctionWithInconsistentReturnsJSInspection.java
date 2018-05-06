package com.sixrr.inspectjs.validity;

import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.JavaScriptSupportLoader;
import com.intellij.psi.PsiElement;
import com.intellij.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.utils.ControlFlowUtils;

public class FunctionWithInconsistentReturnsJSInspection extends JavaScriptInspection {

    @Override
	@Nonnull
    public String getDisplayName() {
        return InspectionJSBundle.message("function.with.inconsistent.returns.display.name");
    }

    @Override
	@Nonnull
    public String getGroupDisplayName() {
        return JSGroupNames.VALIDITY_GROUP_NAME;
    }

    @Override
	public boolean isEnabledByDefault() {
        return true;
    }

    @Override
	@Nullable
    protected String buildErrorString(Object... args) {
        final JSFunction function = (JSFunction) ((PsiElement) args[0]).getParent();
        assert function != null;
        if (functionHasIdentifier(function)) {
            return InspectionJSBundle.message("function.has.inconsistent.return.points.error.string");
        } else {
            return InspectionJSBundle.message("anonymous.function.has.inconsistent.return.points.error.string");
        }
    }

    @Override
	public BaseInspectionVisitor buildVisitor() {
        return new Visitor();
    }

    private static class Visitor extends BaseInspectionVisitor {
        @Override
		protected ProblemHighlightType getProblemHighlightType(PsiElement location) {
            return location.getContainingFile().getLanguage() == JavaScriptSupportLoader.ECMA_SCRIPT_L4 ?
                   ProblemHighlightType.GENERIC_ERROR:super.getProblemHighlightType(location);
        }
        @Override public void visitJSFunctionDeclaration(JSFunction function) {
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
        final ReturnValuesVisitor visitor = new ReturnValuesVisitor(function);
        function.accept(visitor);
        return visitor.hasReturnValues();
    }

    private static boolean functionHasValuelessReturns(JSFunction function) {
        final PsiElement lastChild = function.getLastChild();
        if (lastChild instanceof JSBlockStatement) {
            if (ControlFlowUtils.statementMayCompleteNormally((JSStatement) lastChild)) {
                return true;
            }
        }
        final ValuelessReturnVisitor visitor = new ValuelessReturnVisitor(function);
        function.acceptChildren(visitor);
        return visitor.hasValuelessReturns();
    }

    private static class ReturnValuesVisitor extends JSRecursiveElementVisitor {
        private final JSFunction function;
        private boolean hasReturnValues = false;

        ReturnValuesVisitor(JSFunction function) {
            this.function = function;
        }

        @Override public void visitJSReturnStatement(JSReturnStatement statement) {
            super.visitJSReturnStatement(statement);
            if (statement.getExpression() != null) {
                final JSFunction containingFunction =
                        PsiTreeUtil.getParentOfType(statement, JSFunction.class);
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

        @Override public void visitJSReturnStatement(JSReturnStatement statement) {
            super.visitJSReturnStatement(statement);
            if (statement.getExpression() == null) {
                final JSFunction containingFunction =
                        PsiTreeUtil.getParentOfType(statement, JSFunction.class);
                if (function.equals(containingFunction)) {
                    hasValuelessReturns = true;
                }
            }
        }

        @Override public void visitJSFunctionDeclaration(JSFunction function) {
            // do nothing, so that it doesn't drill into nested functions
        }

        public boolean hasValuelessReturns() {
            return hasValuelessReturns;
        }
    }
}
