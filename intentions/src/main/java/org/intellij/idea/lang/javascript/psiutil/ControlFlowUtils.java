/*
 * Copyright 2005-2006 Olivier Descout
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.intellij.idea.lang.javascript.psiutil;

import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.util.PsiTreeUtil;
import javax.annotation.Nonnull;
import javax.annotation.Nullable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ControlFlowUtils {
    private ControlFlowUtils() {}

    public static boolean statementMayCompleteNormally(@Nullable JSStatement statement) {
        if (statement == null                        ||
            statement instanceof JSBreakStatement    ||
            statement instanceof JSContinueStatement ||
            statement instanceof JSReturnStatement   ||
            statement instanceof JSThrowStatement    ||
            statement instanceof JSWithStatement) {
            return false;
        } else if (statement instanceof JSExpressionStatement ||
                   statement instanceof JSEmptyStatement      ||
                   statement instanceof JSVarStatement) {
            return true;
        } else if (statement instanceof JSForStatement) {
            final JSForStatement loopStatement = (JSForStatement) statement;
            final JSExpression   condition     = loopStatement.getCondition();

            return (condition != null && !isBooleanConstant(condition, false) ||
                    statementIsBreakTarget(loopStatement));
        } else if (statement instanceof JSForInStatement) {
            return (statementIsBreakTarget(statement));
        } else if (statement instanceof JSWhileStatement) {
            final JSWhileStatement loopStatement = (JSWhileStatement) statement;

            return (!isBooleanConstant(loopStatement.getCondition(), true) ||
                    statementIsBreakTarget(loopStatement));
        } else if (statement instanceof JSDoWhileStatement) {
            final JSDoWhileStatement loopStatement = (JSDoWhileStatement) statement;

            return (statementMayCompleteNormally(loopStatement.getBody()) &&
                    (!isBooleanConstant(loopStatement.getCondition(), true) ||
                     statementIsBreakTarget(loopStatement)));
        } else if (statement instanceof JSBlockStatement) {
            return codeBlockMayCompleteNormally(((JSBlockStatement) statement).getStatements());
        } else if (statement instanceof JSLabeledStatement) {
            final JSLabeledStatement labeledStatement = (JSLabeledStatement) statement;
            final JSStatement body = labeledStatement.getStatement();

            return (statementMayCompleteNormally(body) || statementIsBreakTarget(body));
        } else if (statement instanceof JSIfStatement) {
            final JSIfStatement ifStatement = (JSIfStatement) statement;
            final JSStatement   thenBranch  = ifStatement.getThen();
            final JSStatement   elseBranch  = ifStatement.getElse();

            return (elseBranch == null ||
                    statementMayCompleteNormally(thenBranch) ||
                    statementMayCompleteNormally(elseBranch));
        } else if (statement instanceof JSTryStatement) {
            final JSTryStatement tryStatement = (JSTryStatement) statement;

            if (!statementMayCompleteNormally(tryStatement.getFinallyStatement())) {
                return false;
            }
            if (statementMayCompleteNormally(tryStatement.getStatement())) {
                return true;
            }

            final JSCatchBlock catchBlock = tryStatement.getCatchBlock();

            return (catchBlock != null && statementMayCompleteNormally(catchBlock.getStatement()));
        } else if (statement instanceof JSSwitchStatement) {
            final JSSwitchStatement switchStatement = (JSSwitchStatement) statement;

            if (statementIsBreakTarget(switchStatement)) {
                return true;
            }

            for (JSCaseClause caseClause : switchStatement.getCaseClauses()) {
                if (!codeBlockMayCompleteNormally(caseClause.getStatements())) {
                    return false;
                }
            }
            return true;
        } else {
            return false;
        }
    }

    public static boolean codeBlockMayCompleteNormally(@Nullable JSStatement[] statements) {
        if (statements != null) {
            for (final JSStatement statement : statements) {
                if (!statementMayCompleteNormally(statement)) {
                    return false;
                }
            }
        }
        return true;
    }

    @SuppressWarnings({"UnnecessaryUnboxing"})
    private static boolean isBooleanConstant(JSExpression test, boolean val) {
        if (!ExpressionUtil.isConstantExpression(test)) {
            return false;
        }
        final Object value = ExpressionUtil.computeConstantExpression(test);
        return (value != null &&
                value instanceof Boolean &&
                ((Boolean) value).booleanValue() == val);
    }

    private static boolean statementIsBreakTarget(@Nullable JSStatement statement) {
        if (statement == null) {
            return false;
        }

        final BreakTargetFinder breakFinder = new BreakTargetFinder(statement);

        statement.accept(breakFinder);
        return breakFinder.breakFound();
    }

    public static boolean statementContainsExitingBreak(@Nullable JSStatement statement) {
        if (statement == null) {
            return false;
        }

        final ExitingBreakFinder breakFinder = new ExitingBreakFinder();

        statement.accept(breakFinder);
        return breakFinder.breakFound();
    }

    public static boolean statementIsContinueTarget(@Nullable JSStatement statement) {
        if (statement == null) {
            return false;
        }

        final ContinueTargetFinder continueFinder = new ContinueTargetFinder(statement);

        statement.accept(continueFinder);
        return continueFinder.continueFound();
    }

    public static boolean statementContainsReturn(@Nullable JSStatement statement) {
        if (statement == null) {
            return false;
        }

        final ReturnFinder returnFinder = new ReturnFinder();

        statement.accept(returnFinder);
        return returnFinder.returnFound();
    }

    public static boolean statementCompletesWithStatement(
            @Nonnull JSStatement containingStatement,
            @Nonnull JSStatement statement) {
        JSElement statementToCheck = statement;

        while (true) {
            if (statementToCheck.equals(containingStatement)) {
                return true;
            }

            final JSStatement container = PsiTreeUtil.getParentOfType(statementToCheck, JSStatement.class);

            if (container == null) {
                return false;
            }
            if (container instanceof JSBlockStatement) {
                if (!statementIsLastInBlock((JSBlockStatement) container,
                                            (JSStatement) statementToCheck)) {
                    return false;
                }
            }
            if (container instanceof JSLoopStatement) {
                return false;
            }
            statementToCheck = container;
        }
    }

    private static boolean elementCompletesWithStatement(
            @Nonnull JSElement   element,
            @Nonnull JSStatement statement) {
        PsiElement statementToCheck = statement;

        while (true) {
            if (statementToCheck == null) {
                return false;
            }
            if (statementToCheck.equals(element)) {
                return true;
            }

            final JSElement container = PsiTreeUtil.getParentOfType(statementToCheck, JSElement.class);

            if (container == null || container instanceof JSLoopStatement) {
                return false;
            }

            if (container instanceof JSBlockStatement) {
                if (!statementIsLastInBlock((JSBlockStatement) container,
                                            (JSStatement) statementToCheck)) {
                    return false;
                }
                if (container.equals(element)) {
                    return true;
                }
                statementToCheck = PsiTreeUtil.getParentOfType(container, JSStatement.class, JSFunction.class);
            } else {
                statementToCheck = container;
            }
        }
    }

    public static boolean functionCompletesWithStatement(
            @Nonnull JSFunction  function,
            @Nonnull JSStatement statement) {
        return elementCompletesWithStatement(function, statement);
    }

    public static boolean blockCompletesWithStatement(
            @Nonnull JSBlockStatement block,
            @Nonnull JSStatement      statement) {
        return elementCompletesWithStatement(block, statement);
    }

    private static boolean statementIsLastInBlock(
            @Nonnull JSBlockStatement block, @Nonnull JSStatement statement) {
        final JSStatement[] statements = block.getStatements();

        //noinspection ForLoopWithMissingComponent
        for (int index = statements.length; --index >= 0; ) {
            final JSStatement childStatement = statements[index];

            if (statement.equals(childStatement)) {
                return true;
            }
            if (!(statement instanceof JSEmptyStatement)) {
                return false;
            }
        }
        return false;
    }

    @Nullable public static JSVariable resolveVariable(@Nullable JSExpression expression) {
        if (expression == null) {
            return null;
        } else if (expression instanceof JSReferenceExpression) {
            final PsiElement variable = ((JSReferenceExpression) expression).resolve();

            return ((variable != null && variable instanceof JSVariable) ? (JSVariable) variable : null);
        } else if (expression instanceof JSDefinitionExpression) {
            final JSExpression referentExpression = ((JSDefinitionExpression) expression).getExpression();
            final PsiReference reference          = (referentExpression == null) ? null : referentExpression.getReference();
            final PsiElement   variable           = (reference == null)          ? null : reference.resolve();

            return ((variable != null && variable instanceof JSVariable) ? (JSVariable) variable : null);
        } else {
            return null;
        }
    }

    @Nullable public static JSFunction resolveMethod(JSCallExpression expression) {
        final JSExpression methodExpression = (expression == null) ? null : expression.getMethodExpression();

        if (methodExpression != null && methodExpression instanceof JSReferenceExpression) {
            final PsiElement referent = ((JSReferenceExpression) methodExpression).resolve();

            return ((referent != null && referent instanceof JSFunction) ? (JSFunction) referent : null);
        }
        return null;
    }

    public static boolean canBeMerged(JSStatement statement1,
                                      JSStatement statement2) {
        if (!ControlFlowUtils.statementMayCompleteNormally(statement1)) {
            return false;
        }
        final Set<String> statement1Declarations = calculateTopLevelDeclarations(statement1);
        if (containsConflictingDeclarations(statement1Declarations, statement2)) {
            return false;
        }
        final Set<String> statement2Declarations = calculateTopLevelDeclarations(statement2);
        return (!containsConflictingDeclarations(statement2Declarations, statement1));
    }

    public static boolean isInLoopStatementBody(@Nonnull PsiElement element) {
        final JSLoopStatement forStatement = PsiTreeUtil.getParentOfType(element, JSLoopStatement.class);

        if (forStatement == null) {
            return false;
        }

        final JSStatement body = forStatement.getBody();

        return (body != null && PsiTreeUtil.isAncestor(body, element, true));
    }

    @Nonnull
	public static List<JSCallExpression> getRecursiveCalls(@Nonnull JSFunction function) {
        final RecursiveCallVisitor recursiveCallVisitor = new RecursiveCallVisitor(function);

        function.accept(recursiveCallVisitor);
        return recursiveCallVisitor.getCalls();
    }

    private static boolean containsConflictingDeclarations(Set<String>  declarations,
                                                           JSStatement statement) {
        final DeclarationUtils.DeclarationConflictVisitor visitor =
                new DeclarationUtils.DeclarationConflictVisitor(declarations);
        statement.accept(visitor);
        return visitor.hasConflict();
    }

    public static boolean containsConflictingDeclarations(
            JSBlockStatement block, JSBlockStatement parentBlock) {
        final JSStatement[]   statements   = block.getStatements();
        final Set<JSVariable> declaredVars = new HashSet<JSVariable>();

        for (final JSStatement statement : statements) {
            if (statement instanceof JSVarStatement) {
                final JSVarStatement declaration = (JSVarStatement) statement;

                for (final JSVariable var : declaration.getVariables()) {
                    declaredVars.add(var);
                }
            }
        }

        for (final JSVariable variable : declaredVars) {
            if (conflictingDeclarationExists(variable.getName(), parentBlock, block)) {
                return true;
            }
        }

        return false;
    }

    private static boolean conflictingDeclarationExists(
            String           name,
            JSBlockStatement parentBlock,
            JSBlockStatement exceptBlock) {
        final ConflictingDeclarationVisitor visitor = new ConflictingDeclarationVisitor(name, exceptBlock);
        parentBlock.accept(visitor);
        return visitor.hasConflictingDeclaration();
    }

    private static Set<String> calculateTopLevelDeclarations(JSStatement statement) {
        final Set<String> out = new HashSet<String>();

        if (statement instanceof JSVarStatement) {
            addDeclarations((JSVarStatement) statement, out);
        } else if (statement instanceof JSBlockStatement) {
            for (JSStatement subStatement : ((JSBlockStatement) statement).getStatements()) {
                if (subStatement instanceof JSVarStatement) {
                    addDeclarations((JSVarStatement) subStatement, out);
                }
            }
        }
        return out;
    }

    private static void addDeclarations(JSVarStatement statement,
                                        Set<String>    declaredVars) {
        for (final JSVariable variable : statement.getVariables()) {
            declaredVars.add(variable.getName());
        }
    }

    public static void appendStatementsInSequence(StringBuilder  buffer,
                                                  JSStatement    statement1,
                                                  JSStatement    statement2) {
        if (statement1 == null) {
            buffer.append(' ')
                  .append(statement2.getText());
        } else if (statement2 == null) {
            buffer.append(' ')
                  .append(statement1.getText());
        } else {
            buffer.append('{');
            appendStatementStripped(buffer, statement1);
            appendStatementStripped(buffer, statement2);
            buffer.append('}');
        }
    }

    private static void appendStatementStripped(StringBuilder buffer, JSStatement statement) {
        if (statement instanceof JSBlockStatement) {
            for (PsiElement child : statement.getChildren()) {
                buffer.append(child.getText());
            }
        } else {
            buffer.append(statement.getText());
        }
    }

    private static class BreakTargetFinder extends JSRecursiveElementVisitor {
        private boolean           found;
        private final JSStatement target;

        private BreakTargetFinder(JSStatement target) {
            this.target = target;
        }

        private boolean breakFound() {
            return this.found;
        }

        @Override public void visitJSReferenceExpression(JSReferenceExpression JSReferenceExpression) {}

        @Override public void visitJSBreakStatement(JSBreakStatement breakStatement) {
            super.visitJSBreakStatement(breakStatement);
            final JSStatement exitedStatement = breakStatement.getStatementToBreak();
            if (exitedStatement == null) {
                return;
            }
            this.found = (exitedStatement.equals(this.target));
        }
    }

    private static class ContinueTargetFinder extends JSRecursiveElementVisitor {
        private boolean           found;
        private final JSStatement target;

        private ContinueTargetFinder(JSStatement target) {
            this.target = target;
        }

        private boolean continueFound() {
            return this.found;
        }

        @Override public void visitJSReferenceExpression(JSReferenceExpression JSReferenceExpression) {}

        @Override public void visitJSContinueStatement(JSContinueStatement continueStatement) {
            super.visitJSContinueStatement(continueStatement);
            final JSStatement statement = continueStatement.getStatementToContinue();
            if (statement == null) {
                return;
            }
            this.found = (statement.equals(this.target));
        }
    }

    private static class ExitingBreakFinder extends JSRecursiveElementVisitor {
        private boolean found;

        private ExitingBreakFinder() {}

        private boolean breakFound() {
            return this.found;
        }

        @Override public void visitJSReferenceExpression(JSReferenceExpression exp) {}

        @Override public void visitJSBreakStatement(JSBreakStatement breakStatement) {
            if (breakStatement.getLabel() != null) {
                return;
            }
            this.found = true;
        }

        @Override public void visitJSDoWhileStatement(JSDoWhileStatement statement) {
            // don't drill down
        }

        @Override public void visitJSForStatement(JSForStatement statement) {
            // don't drill down
        }

        @Override public void visitJSForInStatement(JSForInStatement statement) {
            // don't drill down
        }

        @Override public void visitJSWhileStatement(JSWhileStatement statement) {
            // don't drill down
        }

        @Override public void visitJSSwitchStatement(JSSwitchStatement statement) {
            // don't drill down
        }
    }

    private static class ReturnFinder extends JSRecursiveElementVisitor {
        private boolean found;

        private ReturnFinder() {}

        private boolean returnFound() {
            return this.found;
        }

        @Override public void visitJSReturnStatement(JSReturnStatement returnStatement) {
            this.found = true;
        }
    }

    private static class ConflictingDeclarationVisitor extends JSRecursiveElementVisitor {

        private final String           variableName;
        private final JSBlockStatement exceptBlock;
        private       boolean          hasConflictingDeclaration;

        ConflictingDeclarationVisitor(String           variableName,
                                      JSBlockStatement exceptBlock) {
            this.variableName = variableName;
            this.exceptBlock  = exceptBlock;
        }

        @Override public void visitJSElement(JSElement element) {
            if (!this.hasConflictingDeclaration) {
                super.visitJSElement(element);
            }
        }

        @Override public void visitJSBlock(JSBlockStatement block) {
            if (!(this.hasConflictingDeclaration ||
                  block.equals(this.exceptBlock))) {
                super.visitJSBlock(block);
            }
        }

        @Override public void visitJSVariable(JSVariable variable) {
            if (!this.hasConflictingDeclaration) {
                super.visitJSVariable(variable);
                final String name = variable.getName();
                this.hasConflictingDeclaration = (name != null && name.equals(this.variableName));
            }
        }

        public boolean hasConflictingDeclaration() {
            return this.hasConflictingDeclaration;
        }
    }

    private static class RecursiveCallVisitor extends JSRecursiveElementVisitor {

        private final JSFunction       function;
        private List<JSCallExpression> recursionReturns;

        RecursiveCallVisitor(JSFunction function) {
            this.function         = function;
            this.recursionReturns = new ArrayList<JSCallExpression>();
        }

        @Override public void visitJSCallExpression(JSCallExpression callExpression) {
            super.visitJSCallExpression(callExpression);

            final JSExpression methodExpression = callExpression.getMethodExpression();

            if (methodExpression == null || !(methodExpression instanceof JSReferenceExpression)) {
                return;
            }

            final JSReturnStatement returnStatement = PsiTreeUtil.getParentOfType(callExpression, JSReturnStatement.class);

            if (returnStatement == null) {
                return;
            }

            final PsiElement calledFunction = ((JSReferenceExpression) methodExpression).resolve();

            if (calledFunction == null ||
                (!(calledFunction instanceof JSFunction) || !calledFunction.equals(this.function))) {
                return;
            }


            this.recursionReturns.add(callExpression);
        }

        public List<JSCallExpression> getCalls() {
            return this.recursionReturns;
        }
    }
}