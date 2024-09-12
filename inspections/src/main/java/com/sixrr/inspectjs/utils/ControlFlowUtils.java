package com.sixrr.inspectjs.utils;

import jakarta.annotation.Nonnull;

import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;

import jakarta.annotation.Nullable;

public class ControlFlowUtils {
    private ControlFlowUtils() {
        super();
    }

    public static boolean statementMayCompleteNormally(@Nullable JSStatement statement) {
        if (statement == null) {
            return true;
        }
        if (statement instanceof JSBreakStatement
            || statement instanceof JSContinueStatement
            || statement instanceof JSReturnStatement
            || statement instanceof JSThrowStatement) {
            return false;
        }
        else if (statement instanceof JSExpressionStatement
            || statement instanceof JSEmptyStatement
            || statement instanceof JSVarStatement) {
            return true;
        }
        else if (statement instanceof JSForStatement forStatement) {
            return forStatementMayReturnNormally(forStatement);
        }
        else if (statement instanceof JSForInStatement forInStatement) {
            return foreachStatementMayReturnNormally(forInStatement);
        }
        else if (statement instanceof JSWhileStatement whileStatement) {
            return whileStatementMayReturnNormally(whileStatement);
        }
        else if (statement instanceof JSDoWhileStatement doWhileStatement) {
            return doWhileStatementMayReturnNormally(doWhileStatement);
        }
        else if (statement instanceof JSBlockStatement blockStatement) {
            return blockMayCompleteNormally(blockStatement);
        }
        else if (statement instanceof JSLabeledStatement labeledStatement) {
            return labeledStatementMayCompleteNormally(labeledStatement);
        }
        else if (statement instanceof JSIfStatement ifStatement) {
            return ifStatementMayReturnNormally(ifStatement);
        }
        else if (statement instanceof JSTryStatement tryStatement) {
            return tryStatementMayReturnNormally(tryStatement);
        }
        else if (statement instanceof JSSwitchStatement switchStatement) {
            return switchStatementMayReturnNormally(switchStatement);
        }
        else if (statement instanceof JSWithStatement withStatement) {
            return withStatementMayReturnNormally(withStatement);
        }
        else { // unknown statement type
            return true;
        }
    }

    private static boolean withStatementMayReturnNormally(JSWithStatement jsWithStatement) {
        final JSStatement body = jsWithStatement.getStatement();
        return statementMayCompleteNormally(body);
    }

    private static boolean doWhileStatementMayReturnNormally(@Nonnull JSDoWhileStatement loopStatement) {
        final JSExpression test = loopStatement.getCondition();
        final JSStatement body = loopStatement.getBody();
        return statementMayCompleteNormally(body) && !BoolUtils.isTrue(test) || statementIsBreakTarget(loopStatement);
    }

    private static boolean whileStatementMayReturnNormally(@Nonnull JSWhileStatement loopStatement) {
        final JSExpression test = loopStatement.getCondition();
        return !BoolUtils.isTrue(test) || statementIsBreakTarget(loopStatement);
    }

    private static boolean forStatementMayReturnNormally(@Nonnull JSForStatement loopStatement) {
        final JSExpression test = loopStatement.getCondition();
        if (statementIsBreakTarget(loopStatement)) {
            return true;
        }
        if (test == null) {
            return false;
        }
        return !BoolUtils.isTrue(test);
    }

    private static boolean foreachStatementMayReturnNormally(@Nonnull JSForInStatement loopStatement) {
        return true;
    }

    private static boolean switchStatementMayReturnNormally(@Nonnull JSSwitchStatement switchStatement) {
        if (statementIsBreakTarget(switchStatement)) {
            return true;
        }
        final JSCaseClause[] caseClauses = switchStatement.getCaseClauses();

        if (caseClauses.length == 0) {
            return true;
        }
        boolean hasDefaultCase = false;
        for (JSCaseClause clause : caseClauses) {
            if (clause.isDefault()) {
                hasDefaultCase = true;
            }
        }

        if (!hasDefaultCase) {
            return true;
        }
        final JSCaseClause lastClause = caseClauses[caseClauses.length - 1];
        final JSStatement[] statements = lastClause.getStatements();
        if (statements.length == 0) {
            return true;
        }
        return statementMayCompleteNormally(statements[statements.length - 1]);
    }

    private static boolean tryStatementMayReturnNormally(@Nonnull JSTryStatement tryStatement) {
        final JSStatement finallyBlock = tryStatement.getFinallyStatement();
        if (finallyBlock != null && !statementMayCompleteNormally(finallyBlock)) {
            return false;
        }
        final JSStatement tryBlock = tryStatement.getStatement();
        if (statementMayCompleteNormally(tryBlock)) {
            return true;
        }
        final JSCatchBlock catchBlock = tryStatement.getCatchBlock();
        if (catchBlock == null) {
            return false;
        }
        final JSStatement catchStatement = catchBlock.getStatement();
        return statementMayCompleteNormally(catchStatement);
    }

    private static boolean ifStatementMayReturnNormally(@Nonnull JSIfStatement ifStatement) {
        final JSStatement thenBranch = ifStatement.getThen();
        if (statementMayCompleteNormally(thenBranch)) {
            return true;
        }
        final JSStatement elseBranch = ifStatement.getElse();
        return elseBranch == null || statementMayCompleteNormally(elseBranch);
    }

    private static boolean labeledStatementMayCompleteNormally(@Nonnull JSLabeledStatement labeledStatement) {
        final JSStatement statement = labeledStatement.getStatement();
        return statementMayCompleteNormally(statement) || statementIsBreakTarget(statement);
    }

    public static boolean blockMayCompleteNormally(@Nullable JSBlockStatement block) {
        if (block == null) {
            return true;
        }
        final JSStatement[] statements = block.getStatements();
        for (final JSStatement statement : statements) {
            if (!statementMayCompleteNormally(statement)) {
                return false;
            }
        }
        return true;
    }

    private static boolean statementIsBreakTarget(@Nonnull JSStatement statement) {
        final BreakFinder breakFinder = new BreakFinder(statement);
        statement.accept(breakFinder);
        return breakFinder.breakFound();
    }

    public static boolean statementContainsReturn(@Nonnull JSStatement statement) {
        final ReturnFinder returnFinder = new ReturnFinder();
        statement.accept(returnFinder);
        return returnFinder.returnFound();
    }

    public static boolean statementIsContinueTarget(@Nonnull JSStatement statement) {
        final ContinueFinder continueFinder = new ContinueFinder(statement);
        statement.accept(continueFinder);
        return continueFinder.continueFound();
    }

    public static boolean isInLoop(@Nonnull JSElement element) {
        return isInForStatementBody(element)
            || isInForeachStatementBody(element)
            || isInWhileStatementBody(element)
            || isInDoWhileStatementBody(element);
    }

    public static boolean isInFinallyBlock(@Nonnull JSElement element) {
        JSElement currentElement = element;
        while (true) {
            final JSTryStatement tryStatement = PsiTreeUtil.getParentOfType(currentElement, JSTryStatement.class);
            if (tryStatement == null) {
                return false;
            }
            final JSStatement finallyBlock = tryStatement.getFinallyStatement();
            if (finallyBlock != null) {
                if (PsiTreeUtil.isAncestor(finallyBlock, currentElement, true)) {
                    return true;
                }
            }
            currentElement = tryStatement;
        }
    }

    public static boolean isInCatchBlock(@Nonnull JSElement element) {
        JSElement currentElement = element;
        while (true) {
            final JSTryStatement tryStatement = PsiTreeUtil.getParentOfType(currentElement, JSTryStatement.class);
            if (tryStatement == null) {
                return false;
            }
            final JSCatchBlock catchBlock = tryStatement.getCatchBlock();
            if (catchBlock != null) {
                final JSStatement catchStatement = catchBlock.getStatement();
                if (PsiTreeUtil.isAncestor(catchStatement, currentElement, true)) {
                    return true;
                }
            }
            currentElement = tryStatement;
        }
    }

    private static boolean isInWhileStatementBody(@Nonnull JSElement element) {
        final JSWhileStatement whileStatement = PsiTreeUtil.getParentOfType(element, JSWhileStatement.class);
        if (whileStatement == null) {
            return false;
        }
        final JSStatement body = whileStatement.getBody();
        return PsiTreeUtil.isAncestor(body, element, true);
    }

    private static boolean isInDoWhileStatementBody(@Nonnull JSElement element) {
        final JSDoWhileStatement doWhileStatement = PsiTreeUtil.getParentOfType(element, JSDoWhileStatement.class);
        if (doWhileStatement == null) {
            return false;
        }
        final JSStatement body = doWhileStatement.getBody();
        return PsiTreeUtil.isAncestor(body, element, true);
    }

    private static boolean isInForStatementBody(@Nonnull JSElement element) {
        final JSForStatement forStatement = PsiTreeUtil.getParentOfType(element, JSForStatement.class);
        if (forStatement == null) {
            return false;
        }
        final JSStatement body = forStatement.getBody();
        return PsiTreeUtil.isAncestor(body, element, true);
    }

    private static boolean isInForeachStatementBody(@Nonnull JSElement element) {
        final JSForInStatement foreachStatement = PsiTreeUtil.getParentOfType(element, JSForInStatement.class);
        if (foreachStatement == null) {
            return false;
        }
        final JSStatement body = foreachStatement.getBody();
        return PsiTreeUtil.isAncestor(body, element, true);
    }

    public static JSStatement stripBraces(@Nonnull JSStatement branch) {
        if (branch instanceof JSBlockStatement block) {
            final JSStatement[] statements = block.getStatements();
            return statements.length == 1 ? statements[0] : block;
        }
        else {
            return branch;
        }
    }

    public static boolean statementCompletesWithStatement(@Nonnull JSStatement containingStatement, @Nonnull JSStatement statement) {
        JSElement statementToCheck = statement;
        while (true) {
            if (statementToCheck.equals(containingStatement)) {
                return true;
            }
            final JSElement container = getContainingStatement(statementToCheck);
            if (container == null) {
                return false;
            }
            if (container instanceof JSBlockStatement containerBlock
                && !statementIsLastInBlock(containerBlock, (JSStatement)statementToCheck)) {
                return false;
            }
            if (isLoop(container) || container instanceof JSSwitchStatement) {
                return false;
            }
            statementToCheck = container;
        }
    }

    public static boolean blockCompletesWithStatement(@Nonnull JSBlockStatement body, @Nonnull JSStatement statement) {
        JSElement statementToCheck = statement;
        while (true) {
            if (statementToCheck == null) {
                return false;
            }
            final JSElement container = getContainingStatement(statementToCheck);
            if (container == null) {
                return false;
            }
            if (isLoop(container)) {
                return false;
            }
            if (container instanceof JSBlockStatement containerBlock) {
                if (!statementIsLastInBlock(containerBlock, (JSStatement)statementToCheck)) {
                    return false;
                }
                if (container.equals(body)) {
                    return true;
                }
                statementToCheck = PsiTreeUtil.getParentOfType(container, JSStatement.class);
            }
            else {
                statementToCheck = container;
            }
        }
    }

    private static boolean isLoop(@Nonnull JSElement element) {
        return element instanceof JSLoopStatement;
    }

    @Nullable
    private static JSElement getContainingStatement(@Nonnull JSElement statement) {
        return PsiTreeUtil.getParentOfType(statement, JSStatement.class);
    }

    private static boolean statementIsLastInBlock(@Nonnull JSBlockStatement block, @Nonnull JSStatement statement) {
        final JSStatement[] statements = block.getStatements();
        for (int i = statements.length - 1; i >= 0; i--) {
            final JSStatement childStatement = statements[i];
            if (statement.equals(childStatement)) {
                return true;
            }
            if (!(statement instanceof JSEmptyStatement)) {
                return false;
            }
        }
        return false;
    }

    private static class ReturnFinder extends JSRecursiveElementVisitor {
        private boolean m_found = false;

        public boolean returnFound() {
            return m_found;
        }

        @Override
        public void visitJSReturnStatement(@Nonnull JSReturnStatement returnStatement) {
            if (m_found) {
                return;
            }
            super.visitJSReturnStatement(returnStatement);
            m_found = true;
        }
    }

    private static class BreakFinder extends JSRecursiveElementVisitor {
        private boolean m_found = false;
        private final JSStatement m_target;

        private BreakFinder(@Nonnull JSStatement target) {
            super();
            m_target = target;
        }

        public boolean breakFound() {
            return m_found;
        }

        @Override
        public void visitJSBreakStatement(@Nonnull JSBreakStatement breakStatement) {
            if (m_found) {
                return;
            }
            super.visitJSBreakStatement(breakStatement);
            final JSStatement exitedStatement = breakStatement.getStatementToBreak();
            if (exitedStatement == null) {
                return;
            }
            if (PsiTreeUtil.isAncestor(exitedStatement, m_target, false)) {
                m_found = true;
            }
        }
    }

    private static class ContinueFinder extends JSRecursiveElementVisitor {
        private boolean m_found = false;
        private final JSStatement m_target;

        private ContinueFinder(@Nonnull JSStatement target) {
            super();
            m_target = target;
        }

        public boolean continueFound() {
            return m_found;
        }

        @Override
        public void visitJSContinueStatement(@Nonnull JSContinueStatement continueStatement) {
            if (m_found) {
                return;
            }
            super.visitJSContinueStatement(continueStatement);
            final JSStatement exitedStatement = continueStatement.getStatementToContinue();
            if (exitedStatement == null) {
                return;
            }
            if (PsiTreeUtil.isAncestor(exitedStatement, m_target, false)) {
                m_found = true;
            }
        }
    }

    public static boolean isInExitStatement(@Nonnull JSExpression expression) {
        return isInReturnStatementArgument(expression) || isInThrowStatementArgument(expression);
    }

    private static boolean isInReturnStatementArgument(@Nonnull JSExpression expression) {
        final JSReturnStatement returnStatement = PsiTreeUtil.getParentOfType(expression, JSReturnStatement.class);
        return returnStatement != null;
    }

    private static boolean isInThrowStatementArgument(@Nonnull JSExpression expression) {
        final JSThrowStatement throwStatement = PsiTreeUtil.getParentOfType(expression, JSThrowStatement.class);
        return throwStatement != null;
    }
}
