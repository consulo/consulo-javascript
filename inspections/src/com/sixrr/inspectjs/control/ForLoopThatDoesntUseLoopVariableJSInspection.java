package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.sixrr.inspectjs.*;
import org.jetbrains.annotations.NotNull;

public class ForLoopThatDoesntUseLoopVariableJSInspection
        extends JavaScriptInspection {

    @NotNull
    public String getDisplayName() {
        return InspectionJSBundle.message(
                "for.loop.not.use.loop.variable.display.name");
    }

    @NotNull
    public String getGroupDisplayName() {
        return JSGroupNames.BUGS_GROUP_NAME;
    }

    @NotNull
    public String buildErrorString(Object... args) {
        final boolean condition = (Boolean) args[0];
        final boolean update = (Boolean) args[1];
        if (condition && update) {
            return InspectionJSBundle.message(
                    "for.loop.not.use.loop.variable.problem.descriptor.both.condition.and.update");
        }
        if (condition) {
            return InspectionJSBundle.message(
                    "for.loop.not.use.loop.variable.problem.descriptor.condition");
        }
        return InspectionJSBundle.message(
                "for.loop.not.use.loop.variable.problem.descriptor.update");
    }

    public BaseInspectionVisitor buildVisitor() {
        return new ForLoopThatDoesntUseLoopVariableVisitor();
    }

    //TODO: right now this only works with a variable declared in the for loop
    //TODO: make it work with an implicit declaration as well
    private static class ForLoopThatDoesntUseLoopVariableVisitor
            extends BaseInspectionVisitor {

        @Override public void visitJSForStatement(@NotNull JSForStatement statement) {
            super.visitJSForStatement(statement);
            if (conditionUsesInitializer(statement)) {
                if (!updateUsesInitializer(statement)) {
                    registerStatementError(statement,
                            Boolean.FALSE, Boolean.TRUE);
                }
            } else {
                if (updateUsesInitializer(statement)) {
                    registerStatementError(statement,
                            Boolean.TRUE, Boolean.FALSE);
                } else {
                    registerStatementError(statement,
                            Boolean.TRUE, Boolean.TRUE);
                }
            }
        }

        private static boolean conditionUsesInitializer(
                JSForStatement statement) {
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

        private static boolean expressionUsesVariable(JSExpression expression,
                                                      JSVariable localVar) {
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

        @Override public void visitElement(@NotNull PsiElement element) {
            if (!used) {
                super.visitElement(element);
            }
        }

        @Override public void visitJSReferenceExpression(
                @NotNull JSReferenceExpression ref) {
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