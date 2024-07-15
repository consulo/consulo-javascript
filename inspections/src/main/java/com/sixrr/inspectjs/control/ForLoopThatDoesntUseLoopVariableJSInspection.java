package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.*;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.psi.PsiElement;

import jakarta.annotation.Nonnull;

@ExtensionImpl
public class ForLoopThatDoesntUseLoopVariableJSInspection extends JavaScriptInspection {
    @Override
    @Nonnull
    public String getDisplayName() {
        return InspectionJSLocalize.forLoopNotUseLoopVariableDisplayName().get();
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
        final boolean condition = (Boolean) args[0];
        final boolean update = (Boolean) args[1];
        if (condition && update) {
            return InspectionJSLocalize.forLoopNotUseLoopVariableProblemDescriptorBothConditionAndUpdate().get();
        }
        if (condition) {
            return InspectionJSLocalize.forLoopNotUseLoopVariableProblemDescriptorCondition().get();
        }
        return InspectionJSLocalize.forLoopNotUseLoopVariableProblemDescriptorUpdate().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new ForLoopThatDoesntUseLoopVariableVisitor();
    }

    //TODO: right now this only works with a variable declared in the for loop
    //TODO: make it work with an implicit declaration as well
    private static class ForLoopThatDoesntUseLoopVariableVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSForStatement(@Nonnull JSForStatement statement) {
            super.visitJSForStatement(statement);
            if (conditionUsesInitializer(statement)) {
                if (!updateUsesInitializer(statement)) {
                    registerStatementError(statement, Boolean.FALSE, Boolean.TRUE);
                }
            } else {
                if (updateUsesInitializer(statement)) {
                    registerStatementError(statement, Boolean.TRUE, Boolean.FALSE);
                } else {
                    registerStatementError(statement, Boolean.TRUE, Boolean.TRUE);
                }
            }
        }

        private static boolean conditionUsesInitializer(JSForStatement statement) {
            final JSExpression condition = statement.getCondition();
            if (condition == null) {
                return true;
            }
            final JSVarStatement initialization = statement.getVarDeclaration();
            if (initialization == null) {
                return true;
            }
            final JSVariable[] variables = initialization.getVariables();
            if(variables.length !=1)
            {
                return false;
            }
            return expressionUsesVariable(condition, variables[0]);
        }

        private static boolean updateUsesInitializer(JSForStatement statement) {
            final JSExpression update = statement.getUpdate();
            if (update == null) {
                return true;
            }
            final JSVarStatement initialization = statement.getVarDeclaration();
            if (initialization == null) {
                return true;
            }
            final JSVariable[] variables = initialization.getVariables();
            if (variables.length != 1) {
                return false;
            }
            return expressionUsesVariable(update, variables[0]);
        }

        private static boolean expressionUsesVariable(JSExpression expression, JSVariable localVar) {
            final UseVisitor useVisitor = new UseVisitor(localVar);
            expression.accept(useVisitor);
            return useVisitor.isUsed();
        }
    }

    private static class UseVisitor extends JSRecursiveElementVisitor {
        private final JSVariable variable;
        private boolean used = false;

        private UseVisitor(JSVariable var) {
            super();
            variable = var;
        }

        @Override public void visitElement(@Nonnull PsiElement element) {
            if (!used) {
                super.visitElement(element);
            }
        }

        @Override public void visitJSReferenceExpression(
                @Nonnull JSReferenceExpression ref) {
            if (used) {
                return;
            }
            super.visitJSReferenceExpression(ref);
            final PsiElement resolvedElement = ref.resolve();
            if (variable.equals(resolvedElement)) {
                used = true;
            }
        }

        public boolean isUsed() {
            return used;
        }
    }
}