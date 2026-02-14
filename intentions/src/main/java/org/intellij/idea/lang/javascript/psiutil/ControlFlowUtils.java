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
import consulo.language.psi.PsiElement;
import consulo.language.psi.PsiReference;
import consulo.language.psi.util.PsiTreeUtil;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class ControlFlowUtils {
    private ControlFlowUtils() {
    }

    public static boolean statementMayCompleteNormally(@Nullable JSStatement statement) {
        if (statement == null
            || statement instanceof JSBreakStatement
            || statement instanceof JSContinueStatement
            || statement instanceof JSReturnStatement
            || statement instanceof JSThrowStatement
            || statement instanceof JSWithStatement) {
            return false;
        }
        else if (statement instanceof JSExpressionStatement
            || statement instanceof JSEmptyStatement
            || statement instanceof JSVarStatement) {
            return true;
        }
        else if (statement instanceof JSForStatement loopStatement) {
            JSExpression condition = loopStatement.getCondition();

            return condition != null && !isBooleanConstant(condition, false) || statementIsBreakTarget(loopStatement);
        }
        else if (statement instanceof JSForInStatement) {
            return statementIsBreakTarget(statement);
        }
        else if (statement instanceof JSWhileStatement loopStatement) {
            return !isBooleanConstant(loopStatement.getCondition(), true) || statementIsBreakTarget(loopStatement);
        }
        else if (statement instanceof JSDoWhileStatement loopStatement) {
            return statementMayCompleteNormally(loopStatement.getBody())
                && (!isBooleanConstant(loopStatement.getCondition(), true) || statementIsBreakTarget(loopStatement));
        }
        else if (statement instanceof JSBlockStatement blockStatement) {
            return codeBlockMayCompleteNormally(blockStatement.getStatements());
        }
        else if (statement instanceof JSLabeledStatement) {
            JSLabeledStatement labeledStatement = (JSLabeledStatement)statement;
            JSStatement body = labeledStatement.getStatement();

            return (statementMayCompleteNormally(body) || statementIsBreakTarget(body));
        }
        else if (statement instanceof JSIfStatement ifStatement) {
            JSStatement thenBranch = ifStatement.getThen();
            JSStatement elseBranch = ifStatement.getElse();

            return elseBranch == null ||
                statementMayCompleteNormally(thenBranch) ||
                statementMayCompleteNormally(elseBranch);
        }
        else if (statement instanceof JSTryStatement tryStatement) {
            if (!statementMayCompleteNormally(tryStatement.getFinallyStatement())) {
                return false;
            }
            if (statementMayCompleteNormally(tryStatement.getStatement())) {
                return true;
            }

            JSCatchBlock catchBlock = tryStatement.getCatchBlock();

            return catchBlock != null && statementMayCompleteNormally(catchBlock.getStatement());
        }
        else if (statement instanceof JSSwitchStatement switchStatement) {
            if (statementIsBreakTarget(switchStatement)) {
                return true;
            }

            for (JSCaseClause caseClause : switchStatement.getCaseClauses()) {
                if (!codeBlockMayCompleteNormally(caseClause.getStatements())) {
                    return false;
                }
            }
            return true;
        }
        else {
            return false;
        }
    }

    public static boolean codeBlockMayCompleteNormally(@Nullable JSStatement[] statements) {
        if (statements != null) {
            for (JSStatement statement : statements) {
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
        Object value = ExpressionUtil.computeConstantExpression(test);
        return value != null && value instanceof Boolean && ((Boolean)value).booleanValue() == val;
    }

    private static boolean statementIsBreakTarget(@Nullable JSStatement statement) {
        if (statement == null) {
            return false;
        }

        BreakTargetFinder breakFinder = new BreakTargetFinder(statement);

        statement.accept(breakFinder);
        return breakFinder.breakFound();
    }

    public static boolean statementContainsExitingBreak(@Nullable JSStatement statement) {
        if (statement == null) {
            return false;
        }

        ExitingBreakFinder breakFinder = new ExitingBreakFinder();

        statement.accept(breakFinder);
        return breakFinder.breakFound();
    }

    public static boolean statementIsContinueTarget(@Nullable JSStatement statement) {
        if (statement == null) {
            return false;
        }

        ContinueTargetFinder continueFinder = new ContinueTargetFinder(statement);

        statement.accept(continueFinder);
        return continueFinder.continueFound();
    }

    public static boolean statementContainsReturn(@Nullable JSStatement statement) {
        if (statement == null) {
            return false;
        }

        ReturnFinder returnFinder = new ReturnFinder();

        statement.accept(returnFinder);
        return returnFinder.returnFound();
    }

    public static boolean statementCompletesWithStatement(@Nonnull JSStatement containingStatement, @Nonnull JSStatement statement) {
        JSElement statementToCheck = statement;

        while (true) {
            if (statementToCheck.equals(containingStatement)) {
                return true;
            }

            JSStatement container = PsiTreeUtil.getParentOfType(statementToCheck, JSStatement.class);

            if (container == null) {
                return false;
            }
            if (container instanceof JSBlockStatement blockStatement) {
                if (!statementIsLastInBlock(blockStatement, (JSStatement)statementToCheck)) {
                    return false;
                }
            }
            if (container instanceof JSLoopStatement) {
                return false;
            }
            statementToCheck = container;
        }
    }

    private static boolean elementCompletesWithStatement(@Nonnull JSElement element, @Nonnull JSStatement statement) {
        PsiElement statementToCheck = statement;

        while (true) {
            if (statementToCheck == null) {
                return false;
            }
            if (statementToCheck.equals(element)) {
                return true;
            }

            JSElement container = PsiTreeUtil.getParentOfType(statementToCheck, JSElement.class);

            if (container == null || container instanceof JSLoopStatement) {
                return false;
            }

            if (container instanceof JSBlockStatement blockStatement) {
                if (!statementIsLastInBlock(blockStatement, (JSStatement)statementToCheck)) {
                    return false;
                }
                if (container.equals(element)) {
                    return true;
                }
                statementToCheck = PsiTreeUtil.getParentOfType(container, JSStatement.class, JSFunction.class);
            }
            else {
                statementToCheck = container;
            }
        }
    }

    public static boolean functionCompletesWithStatement(@Nonnull JSFunction function, @Nonnull JSStatement statement) {
        return elementCompletesWithStatement(function, statement);
    }

    public static boolean blockCompletesWithStatement(@Nonnull JSBlockStatement block, @Nonnull JSStatement statement) {
        return elementCompletesWithStatement(block, statement);
    }

    private static boolean statementIsLastInBlock(@Nonnull JSBlockStatement block, @Nonnull JSStatement statement) {
        JSStatement[] statements = block.getStatements();

        //noinspection ForLoopWithMissingComponent
        for (int index = statements.length; --index >= 0; ) {
            JSStatement childStatement = statements[index];

            if (statement.equals(childStatement)) {
                return true;
            }
            if (!(statement instanceof JSEmptyStatement)) {
                return false;
            }
        }
        return false;
    }

    @Nullable
    public static JSVariable resolveVariable(@Nullable JSExpression expression) {
        if (expression == null) {
            return null;
        }
        else if (expression instanceof JSReferenceExpression referenceExpression) {
            PsiElement variable = referenceExpression.resolve();

            return variable != null && variable instanceof JSVariable jsVariable ? jsVariable : null;
        }
        else if (expression instanceof JSDefinitionExpression definitionExpression) {
            JSExpression referentExpression = definitionExpression.getExpression();
            PsiReference reference = (referentExpression == null) ? null : referentExpression.getReference();
            PsiElement variable = (reference == null) ? null : reference.resolve();

            return variable != null && variable instanceof JSVariable jsVariable ? jsVariable : null;
        }
        else {
            return null;
        }
    }

    @Nullable
    public static JSFunction resolveMethod(JSCallExpression expression) {
        JSExpression methodExpression = (expression == null) ? null : expression.getMethodExpression();

        if (methodExpression != null && methodExpression instanceof JSReferenceExpression referenceExpression) {
            PsiElement referent = referenceExpression.resolve();

            return referent != null && referent instanceof JSFunction function ? function : null;
        }
        return null;
    }

    public static boolean canBeMerged(JSStatement statement1, JSStatement statement2) {
        if (!ControlFlowUtils.statementMayCompleteNormally(statement1)) {
            return false;
        }
        Set<String> statement1Declarations = calculateTopLevelDeclarations(statement1);
        if (containsConflictingDeclarations(statement1Declarations, statement2)) {
            return false;
        }
        Set<String> statement2Declarations = calculateTopLevelDeclarations(statement2);
        return (!containsConflictingDeclarations(statement2Declarations, statement1));
    }

    public static boolean isInLoopStatementBody(@Nonnull PsiElement element) {
        JSLoopStatement forStatement = PsiTreeUtil.getParentOfType(element, JSLoopStatement.class);

        if (forStatement == null) {
            return false;
        }

        JSStatement body = forStatement.getBody();

        return (body != null && PsiTreeUtil.isAncestor(body, element, true));
    }

    @Nonnull
    public static List<JSCallExpression> getRecursiveCalls(@Nonnull JSFunction function) {
        RecursiveCallVisitor recursiveCallVisitor = new RecursiveCallVisitor(function);

        function.accept(recursiveCallVisitor);
        return recursiveCallVisitor.getCalls();
    }

    private static boolean containsConflictingDeclarations(Set<String> declarations, JSStatement statement) {
        DeclarationUtils.DeclarationConflictVisitor visitor =
            new DeclarationUtils.DeclarationConflictVisitor(declarations);
        statement.accept(visitor);
        return visitor.hasConflict();
    }

    public static boolean containsConflictingDeclarations(JSBlockStatement block, JSBlockStatement parentBlock) {
        JSStatement[] statements = block.getStatements();
        Set<JSVariable> declaredVars = new HashSet<>();

        for (JSStatement statement : statements) {
            if (statement instanceof JSVarStatement declaration) {
                for (JSVariable var : declaration.getVariables()) {
                    declaredVars.add(var);
                }
            }
        }

        for (JSVariable variable : declaredVars) {
            if (conflictingDeclarationExists(variable.getName(), parentBlock, block)) {
                return true;
            }
        }

        return false;
    }

    private static boolean conflictingDeclarationExists(String name, JSBlockStatement parentBlock, JSBlockStatement exceptBlock) {
        ConflictingDeclarationVisitor visitor = new ConflictingDeclarationVisitor(name, exceptBlock);
        parentBlock.accept(visitor);
        return visitor.hasConflictingDeclaration();
    }

    private static Set<String> calculateTopLevelDeclarations(JSStatement statement) {
        Set<String> out = new HashSet<>();

        if (statement instanceof JSVarStatement varStatement) {
            addDeclarations(varStatement, out);
        }
        else if (statement instanceof JSBlockStatement blockStatement) {
            for (JSStatement subStatement : blockStatement.getStatements()) {
                if (subStatement instanceof JSVarStatement varStatement) {
                    addDeclarations(varStatement, out);
                }
            }
        }
        return out;
    }

    private static void addDeclarations(JSVarStatement statement, Set<String> declaredVars) {
        for (JSVariable variable : statement.getVariables()) {
            declaredVars.add(variable.getName());
        }
    }

    public static void appendStatementsInSequence(StringBuilder buffer, JSStatement statement1, JSStatement statement2) {
        if (statement1 == null) {
            buffer.append(' ')
                .append(statement2.getText());
        }
        else if (statement2 == null) {
            buffer.append(' ')
                .append(statement1.getText());
        }
        else {
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
        }
        else {
            buffer.append(statement.getText());
        }
    }

    private static class BreakTargetFinder extends JSRecursiveElementVisitor {
        private boolean found;
        private final JSStatement target;

        private BreakTargetFinder(JSStatement target) {
            this.target = target;
        }

        private boolean breakFound() {
            return this.found;
        }

        @Override
        public void visitJSReferenceExpression(JSReferenceExpression JSReferenceExpression) {
        }

        @Override
        public void visitJSBreakStatement(JSBreakStatement breakStatement) {
            super.visitJSBreakStatement(breakStatement);
            JSStatement exitedStatement = breakStatement.getStatementToBreak();
            if (exitedStatement == null) {
                return;
            }
            this.found = (exitedStatement.equals(this.target));
        }
    }

    private static class ContinueTargetFinder extends JSRecursiveElementVisitor {
        private boolean found;
        private final JSStatement target;

        private ContinueTargetFinder(JSStatement target) {
            this.target = target;
        }

        private boolean continueFound() {
            return this.found;
        }

        @Override
        public void visitJSReferenceExpression(JSReferenceExpression JSReferenceExpression) {
        }

        @Override
        public void visitJSContinueStatement(JSContinueStatement continueStatement) {
            super.visitJSContinueStatement(continueStatement);
            JSStatement statement = continueStatement.getStatementToContinue();
            if (statement == null) {
                return;
            }
            this.found = (statement.equals(this.target));
        }
    }

    private static class ExitingBreakFinder extends JSRecursiveElementVisitor {
        private boolean found;

        private ExitingBreakFinder() {
        }

        private boolean breakFound() {
            return this.found;
        }

        @Override
        public void visitJSReferenceExpression(JSReferenceExpression exp) {
        }

        @Override
        public void visitJSBreakStatement(JSBreakStatement breakStatement) {
            if (breakStatement.getLabel() != null) {
                return;
            }
            this.found = true;
        }

        @Override
        public void visitJSDoWhileStatement(JSDoWhileStatement statement) {
            // don't drill down
        }

        @Override
        public void visitJSForStatement(JSForStatement statement) {
            // don't drill down
        }

        @Override
        public void visitJSForInStatement(JSForInStatement statement) {
            // don't drill down
        }

        @Override
        public void visitJSWhileStatement(JSWhileStatement statement) {
            // don't drill down
        }

        @Override
        public void visitJSSwitchStatement(JSSwitchStatement statement) {
            // don't drill down
        }
    }

    private static class ReturnFinder extends JSRecursiveElementVisitor {
        private boolean found;

        private ReturnFinder() {
        }

        private boolean returnFound() {
            return this.found;
        }

        @Override
        public void visitJSReturnStatement(JSReturnStatement returnStatement) {
            this.found = true;
        }
    }

    private static class ConflictingDeclarationVisitor extends JSRecursiveElementVisitor {

        private final String variableName;
        private final JSBlockStatement exceptBlock;
        private boolean hasConflictingDeclaration;

        ConflictingDeclarationVisitor(
            String variableName,
            JSBlockStatement exceptBlock
        ) {
            this.variableName = variableName;
            this.exceptBlock = exceptBlock;
        }

        @Override
        public void visitJSElement(JSElement element) {
            if (!this.hasConflictingDeclaration) {
                super.visitJSElement(element);
            }
        }

        @Override
        public void visitJSBlock(JSBlockStatement block) {
            if (!(this.hasConflictingDeclaration ||
                block.equals(this.exceptBlock))) {
                super.visitJSBlock(block);
            }
        }

        @Override
        public void visitJSVariable(JSVariable variable) {
            if (!this.hasConflictingDeclaration) {
                super.visitJSVariable(variable);
                String name = variable.getName();
                this.hasConflictingDeclaration = (name != null && name.equals(this.variableName));
            }
        }

        public boolean hasConflictingDeclaration() {
            return this.hasConflictingDeclaration;
        }
    }

    private static class RecursiveCallVisitor extends JSRecursiveElementVisitor {
        private final JSFunction function;
        private List<JSCallExpression> recursionReturns;

        RecursiveCallVisitor(JSFunction function) {
            this.function = function;
            this.recursionReturns = new ArrayList<>();
        }

        @Override
        public void visitJSCallExpression(JSCallExpression callExpression) {
            super.visitJSCallExpression(callExpression);

            JSExpression methodExpression = callExpression.getMethodExpression();

            if (methodExpression == null || !(methodExpression instanceof JSReferenceExpression)) {
                return;
            }

            JSReturnStatement returnStatement = PsiTreeUtil.getParentOfType(callExpression, JSReturnStatement.class);

            if (returnStatement == null) {
                return;
            }

            PsiElement calledFunction = ((JSReferenceExpression)methodExpression).resolve();

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