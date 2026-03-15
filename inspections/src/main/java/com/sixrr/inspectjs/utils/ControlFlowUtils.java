package com.sixrr.inspectjs.utils;


import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.util.PsiTreeUtil;
import com.sixrr.inspectjs.JSRecursiveElementVisitor;

import org.jspecify.annotations.Nullable;

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
        JSStatement body = jsWithStatement.getStatement();
        return statementMayCompleteNormally(body);
    }

    private static boolean doWhileStatementMayReturnNormally(JSDoWhileStatement loopStatement) {
        JSExpression test = loopStatement.getCondition();
        JSStatement body = loopStatement.getBody();
        return statementMayCompleteNormally(body) && !BoolUtils.isTrue(test) || statementIsBreakTarget(loopStatement);
    }

    private static boolean whileStatementMayReturnNormally(JSWhileStatement loopStatement) {
        JSExpression test = loopStatement.getCondition();
        return !BoolUtils.isTrue(test) || statementIsBreakTarget(loopStatement);
    }

    private static boolean forStatementMayReturnNormally(JSForStatement loopStatement) {
        JSExpression test = loopStatement.getCondition();
        if (statementIsBreakTarget(loopStatement)) {
            return true;
        }
        if (test == null) {
            return false;
        }
        return !BoolUtils.isTrue(test);
    }

    private static boolean foreachStatementMayReturnNormally(JSForInStatement loopStatement) {
        return true;
    }

    private static boolean switchStatementMayReturnNormally(JSSwitchStatement switchStatement) {
        if (statementIsBreakTarget(switchStatement)) {
            return true;
        }
        JSCaseClause[] caseClauses = switchStatement.getCaseClauses();

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
        JSCaseClause lastClause = caseClauses[caseClauses.length - 1];
        JSStatement[] statements = lastClause.getStatements();
        if (statements.length == 0) {
            return true;
        }
        return statementMayCompleteNormally(statements[statements.length - 1]);
    }

    private static boolean tryStatementMayReturnNormally(JSTryStatement tryStatement) {
        JSStatement finallyBlock = tryStatement.getFinallyStatement();
        if (finallyBlock != null && !statementMayCompleteNormally(finallyBlock)) {
            return false;
        }
        JSStatement tryBlock = tryStatement.getStatement();
        if (statementMayCompleteNormally(tryBlock)) {
            return true;
        }
        JSCatchBlock catchBlock = tryStatement.getCatchBlock();
        if (catchBlock == null) {
            return false;
        }
        JSStatement catchStatement = catchBlock.getStatement();
        return statementMayCompleteNormally(catchStatement);
    }

    private static boolean ifStatementMayReturnNormally(JSIfStatement ifStatement) {
        JSStatement thenBranch = ifStatement.getThen();
        if (statementMayCompleteNormally(thenBranch)) {
            return true;
        }
        JSStatement elseBranch = ifStatement.getElse();
        return elseBranch == null || statementMayCompleteNormally(elseBranch);
    }

    private static boolean labeledStatementMayCompleteNormally(JSLabeledStatement labeledStatement) {
        JSStatement statement = labeledStatement.getStatement();
        return statementMayCompleteNormally(statement) || statementIsBreakTarget(statement);
    }

    public static boolean blockMayCompleteNormally(@Nullable JSBlockStatement block) {
        if (block == null) {
            return true;
        }
        JSStatement[] statements = block.getStatements();
        for (JSStatement statement : statements) {
            if (!statementMayCompleteNormally(statement)) {
                return false;
            }
        }
        return true;
    }

    private static boolean statementIsBreakTarget(JSStatement statement) {
        BreakFinder breakFinder = new BreakFinder(statement);
        statement.accept(breakFinder);
        return breakFinder.breakFound();
    }

    public static boolean statementContainsReturn(JSStatement statement) {
        ReturnFinder returnFinder = new ReturnFinder();
        statement.accept(returnFinder);
        return returnFinder.returnFound();
    }

    public static boolean statementIsContinueTarget(JSStatement statement) {
        ContinueFinder continueFinder = new ContinueFinder(statement);
        statement.accept(continueFinder);
        return continueFinder.continueFound();
    }

    public static boolean isInLoop(JSElement element) {
        return isInForStatementBody(element)
            || isInForeachStatementBody(element)
            || isInWhileStatementBody(element)
            || isInDoWhileStatementBody(element);
    }

    public static boolean isInFinallyBlock(JSElement element) {
        JSElement currentElement = element;
        while (true) {
            JSTryStatement tryStatement = PsiTreeUtil.getParentOfType(currentElement, JSTryStatement.class);
            if (tryStatement == null) {
                return false;
            }
            JSStatement finallyBlock = tryStatement.getFinallyStatement();
            if (finallyBlock != null) {
                if (PsiTreeUtil.isAncestor(finallyBlock, currentElement, true)) {
                    return true;
                }
            }
            currentElement = tryStatement;
        }
    }

    public static boolean isInCatchBlock(JSElement element) {
        JSElement currentElement = element;
        while (true) {
            JSTryStatement tryStatement = PsiTreeUtil.getParentOfType(currentElement, JSTryStatement.class);
            if (tryStatement == null) {
                return false;
            }
            JSCatchBlock catchBlock = tryStatement.getCatchBlock();
            if (catchBlock != null) {
                JSStatement catchStatement = catchBlock.getStatement();
                if (PsiTreeUtil.isAncestor(catchStatement, currentElement, true)) {
                    return true;
                }
            }
            currentElement = tryStatement;
        }
    }

    private static boolean isInWhileStatementBody(JSElement element) {
        JSWhileStatement whileStatement = PsiTreeUtil.getParentOfType(element, JSWhileStatement.class);
        if (whileStatement == null) {
            return false;
        }
        JSStatement body = whileStatement.getBody();
        return PsiTreeUtil.isAncestor(body, element, true);
    }

    private static boolean isInDoWhileStatementBody(JSElement element) {
        JSDoWhileStatement doWhileStatement = PsiTreeUtil.getParentOfType(element, JSDoWhileStatement.class);
        if (doWhileStatement == null) {
            return false;
        }
        JSStatement body = doWhileStatement.getBody();
        return PsiTreeUtil.isAncestor(body, element, true);
    }

    private static boolean isInForStatementBody(JSElement element) {
        JSForStatement forStatement = PsiTreeUtil.getParentOfType(element, JSForStatement.class);
        if (forStatement == null) {
            return false;
        }
        JSStatement body = forStatement.getBody();
        return PsiTreeUtil.isAncestor(body, element, true);
    }

    private static boolean isInForeachStatementBody(JSElement element) {
        JSForInStatement foreachStatement = PsiTreeUtil.getParentOfType(element, JSForInStatement.class);
        if (foreachStatement == null) {
            return false;
        }
        JSStatement body = foreachStatement.getBody();
        return PsiTreeUtil.isAncestor(body, element, true);
    }

    public static JSStatement stripBraces(JSStatement branch) {
        if (branch instanceof JSBlockStatement block) {
            JSStatement[] statements = block.getStatements();
            return statements.length == 1 ? statements[0] : block;
        }
        else {
            return branch;
        }
    }

    public static boolean statementCompletesWithStatement(JSStatement containingStatement, JSStatement statement) {
        JSElement statementToCheck = statement;
        while (true) {
            if (statementToCheck.equals(containingStatement)) {
                return true;
            }
            JSElement container = getContainingStatement(statementToCheck);
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

    public static boolean blockCompletesWithStatement(JSBlockStatement body, JSStatement statement) {
        JSElement statementToCheck = statement;
        while (true) {
            if (statementToCheck == null) {
                return false;
            }
            JSElement container = getContainingStatement(statementToCheck);
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

    private static boolean isLoop(JSElement element) {
        return element instanceof JSLoopStatement;
    }

    @Nullable
    private static JSElement getContainingStatement(JSElement statement) {
        return PsiTreeUtil.getParentOfType(statement, JSStatement.class);
    }

    private static boolean statementIsLastInBlock(JSBlockStatement block, JSStatement statement) {
        JSStatement[] statements = block.getStatements();
        for (int i = statements.length - 1; i >= 0; i--) {
            JSStatement childStatement = statements[i];
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
        public void visitJSReturnStatement(JSReturnStatement returnStatement) {
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

        private BreakFinder(JSStatement target) {
            super();
            m_target = target;
        }

        public boolean breakFound() {
            return m_found;
        }

        @Override
        public void visitJSBreakStatement(JSBreakStatement breakStatement) {
            if (m_found) {
                return;
            }
            super.visitJSBreakStatement(breakStatement);
            JSStatement exitedStatement = breakStatement.getStatementToBreak();
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

        private ContinueFinder(JSStatement target) {
            super();
            m_target = target;
        }

        public boolean continueFound() {
            return m_found;
        }

        @Override
        public void visitJSContinueStatement(JSContinueStatement continueStatement) {
            if (m_found) {
                return;
            }
            super.visitJSContinueStatement(continueStatement);
            JSStatement exitedStatement = continueStatement.getStatementToContinue();
            if (exitedStatement == null) {
                return;
            }
            if (PsiTreeUtil.isAncestor(exitedStatement, m_target, false)) {
                m_found = true;
            }
        }
    }

    public static boolean isInExitStatement(JSExpression expression) {
        return isInReturnStatementArgument(expression) || isInThrowStatementArgument(expression);
    }

    private static boolean isInReturnStatementArgument(JSExpression expression) {
        JSReturnStatement returnStatement = PsiTreeUtil.getParentOfType(expression, JSReturnStatement.class);
        return returnStatement != null;
    }

    private static boolean isInThrowStatementArgument(JSExpression expression) {
        JSThrowStatement throwStatement = PsiTreeUtil.getParentOfType(expression, JSThrowStatement.class);
        return throwStatement != null;
    }
}
