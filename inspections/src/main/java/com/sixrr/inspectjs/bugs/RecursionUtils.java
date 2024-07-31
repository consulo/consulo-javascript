package com.sixrr.inspectjs.bugs;

import jakarta.annotation.Nonnull;

import consulo.language.ast.IElementType;
import com.intellij.lang.javascript.psi.*;
import com.intellij.lang.javascript.JSTokenTypes;
import com.sixrr.inspectjs.utils.BoolUtils;
import consulo.language.psi.PsiElement;

public class RecursionUtils {
    private RecursionUtils() {
        super();
    }

    public static boolean statementMayReturnBeforeRecursing(JSStatement statement, JSFunction function) {
        if (statement == null) {
            return true;
        }
        if (statement instanceof JSBreakStatement
            || statement instanceof JSContinueStatement
            || statement instanceof JSThrowStatement
            || statement instanceof JSExpressionStatement
            || statement instanceof JSEmptyStatement
            || statement instanceof JSVarStatement) {
            return false;
        }
        else if (statement instanceof JSReturnStatement returnStatement) {
            final JSExpression returnValue = returnStatement.getExpression();
            return returnValue == null || !expressionDefinitelyRecurses(returnValue, function);
        }
        else if (statement instanceof JSForStatement forStatement) {
            return forStatementMayReturnBeforeRecursing(forStatement, function);
        }
        else if (statement instanceof JSForInStatement forInStatement) {
            return foreachStatementMayReturnBeforeRecursing(forInStatement, function);
        }
        else if (statement instanceof JSWhileStatement whileStatement) {
            return whileStatementMayReturnBeforeRecursing(whileStatement, function);
        }
        else if (statement instanceof JSDoWhileStatement doWhileStatement) {
            return doWhileStatementMayReturnBeforeRecursing(doWhileStatement, function);
        }
        else if (statement instanceof JSBlockStatement blockStatement) {
            return codeBlockMayReturnBeforeRecursing(blockStatement, function, false);
        }
        else if (statement instanceof JSLabeledStatement labeledStatement) {
            return labeledStatementMayReturnBeforeRecursing(labeledStatement, function);
        }
        else if (statement instanceof JSIfStatement ifStatement) {
            return ifStatementMayReturnBeforeRecursing(ifStatement, function);
        }
        else if (statement instanceof JSTryStatement tryStatement) {
            return tryStatementMayReturnBeforeRecursing(tryStatement, function);
        }
        else if (statement instanceof JSSwitchStatement switchStatement) {
            return switchStatementMayReturnBeforeRecursing(switchStatement, function);
        }
        else {
            // unknown statement type
            return true;
        }
    }

    private static boolean doWhileStatementMayReturnBeforeRecursing(JSDoWhileStatement loopStatement, JSFunction function) {
        final JSStatement body = loopStatement.getBody();
        return statementMayReturnBeforeRecursing(body, function);
    }

    private static boolean whileStatementMayReturnBeforeRecursing(JSWhileStatement loopStatement, JSFunction function) {
        final JSExpression test = loopStatement.getCondition();
        if (expressionDefinitelyRecurses(test, function)) {
            return false;
        }
        final JSStatement body = loopStatement.getBody();
        return statementMayReturnBeforeRecursing(body, function);
    }

    private static boolean forStatementMayReturnBeforeRecursing(JSForStatement loopStatement, JSFunction function) {
        final JSExpression initialization = loopStatement.getInitialization();

        if (expressionDefinitelyRecurses(initialization, function)) {
            return false;
        }
        final JSExpression test = loopStatement.getCondition();
        if (expressionDefinitelyRecurses(test, function)) {
            return false;
        }
        final JSStatement body = loopStatement.getBody();
        return statementMayReturnBeforeRecursing(body, function);
    }

    private static boolean foreachStatementMayReturnBeforeRecursing(JSForInStatement loopStatement, JSFunction function) {
        final JSExpression test = loopStatement.getCollectionExpression();
        if (expressionDefinitelyRecurses(test, function)) {
            return false;
        }
        final JSStatement body = loopStatement.getBody();
        return statementMayReturnBeforeRecursing(body, function);
    }

    private static boolean switchStatementMayReturnBeforeRecursing(JSSwitchStatement switchStatement, JSFunction function) {
        final JSCaseClause[] clauses = switchStatement.getCaseClauses();
        if (clauses == null) {
            return true;
        }
        for (JSCaseClause clause : clauses) {
            final JSStatement[] statements = clause.getStatements();
            for (JSStatement statement : statements) {
                if (statementMayReturnBeforeRecursing(statement, function)) {
                    return true;
                }
            }
        }

        return false;
    }

    private static boolean tryStatementMayReturnBeforeRecursing(JSTryStatement tryStatement, JSFunction function) {
        final JSStatement finallyBlock = tryStatement.getFinallyStatement();
        if (finallyBlock != null) {
            if (statementMayReturnBeforeRecursing(finallyBlock, function)) {
                return true;
            }
            if (statementDefinitelyRecurses(finallyBlock, function)) {
                return false;
            }
        }
        final JSStatement tryBlock = tryStatement.getStatement();
        if (statementMayReturnBeforeRecursing(tryBlock, function)) {
            return true;
        }
        final JSCatchBlock catchBlock = tryStatement.getCatchBlock();
        if (catchBlock == null) {
            return false;
        }
        final JSStatement catchBlockStatement = catchBlock.getStatement();
        return statementMayReturnBeforeRecursing(catchBlockStatement, function);
    }

    private static boolean ifStatementMayReturnBeforeRecursing(JSIfStatement ifStatement, JSFunction function) {
        final JSExpression test = ifStatement.getCondition();
        if (expressionDefinitelyRecurses(test, function)) {
            return false;
        }
        final JSStatement thenBranch = ifStatement.getThen();
        if (statementMayReturnBeforeRecursing(thenBranch, function)) {
            return true;
        }
        final JSStatement elseBranch = ifStatement.getElse();
        return elseBranch != null && statementMayReturnBeforeRecursing(elseBranch, function);
    }

    private static boolean labeledStatementMayReturnBeforeRecursing(JSLabeledStatement labeledStatement, JSFunction function) {
        final JSStatement statement = labeledStatement.getStatement();
        return statementMayReturnBeforeRecursing(statement, function);
    }

    private static boolean codeBlockMayReturnBeforeRecursing(JSBlockStatement block, JSFunction function, boolean endsInImplicitReturn) {
        if (block == null) {
            return true;
        }
        final JSStatement[] statements = block.getStatements();
        for (final JSStatement statement : statements) {
            if (statementMayReturnBeforeRecursing(statement, function)) {
                return true;
            }
            if (statementDefinitelyRecurses(statement, function)) {
                return false;
            }
        }
        return endsInImplicitReturn;
    }

    public static boolean functionMayRecurse(@Nonnull JSFunction function) {
        final RecursionVisitor recursionVisitor = new RecursionVisitor(function);
        function.accept(recursionVisitor);
        return recursionVisitor.isRecursive();
    }

    private static boolean expressionDefinitelyRecurses(JSExpression exp, JSFunction function) {
        if (exp == null) {
            return false;
        }
        if (exp instanceof JSCallExpression callExpression) {
            return functionCallExpressionDefinitelyRecurses(callExpression, function);
        }
        if (exp instanceof JSAssignmentExpression assignmentExpression) {
            return assignmentExpressionDefinitelyRecurses(assignmentExpression, function);
        }
        if (exp instanceof JSArrayLiteralExpression arrayLiteralExpression) {
            return arrayInitializerExpressionDefinitelyRecurses(arrayLiteralExpression, function);
        }
        if (exp instanceof JSPrefixExpression prefixExpression) {
            return prefixExpressionDefinitelyRecurses(prefixExpression, function);
        }
        if (exp instanceof JSPostfixExpression postfixExpression) {
            return postfixExpressionDefinitelyRecurses(postfixExpression, function);
        }
        if (exp instanceof JSBinaryExpression binaryExpression) {
            return binaryExpressionDefinitelyRecurses(binaryExpression, function);
        }
        if (exp instanceof JSConditionalExpression conditionalExpression) {
            return conditionalExpressionDefinitelyRecurses(conditionalExpression, function);
        }
        if (exp instanceof JSParenthesizedExpression parenthesizedExpression) {
            return parenthesizedExpressionDefinitelyRecurses(parenthesizedExpression, function);
        }
        if (exp instanceof JSReferenceExpression referenceExpression) {
            return referenceExpressionDefinitelyRecurses(referenceExpression, function);
        }
        if (exp instanceof JSLiteralExpression || exp instanceof JSThisExpression) {
            return false;
        }
        return false;
    }

    private static boolean conditionalExpressionDefinitelyRecurses(JSConditionalExpression expression, JSFunction function) {
        final JSExpression condExpression = expression.getCondition();
        if (expressionDefinitelyRecurses(condExpression, function)) {
            return true;
        }
        final JSExpression thenExpression = expression.getThen();
        final JSExpression elseExpression = expression.getElse();
        return expressionDefinitelyRecurses(thenExpression, function) && expressionDefinitelyRecurses(elseExpression, function);
    }

    private static boolean binaryExpressionDefinitelyRecurses(JSBinaryExpression expression, JSFunction function) {
        final JSExpression lhs = expression.getLOperand();
        if (expressionDefinitelyRecurses(lhs, function)) {
            return true;
        }
        final IElementType tokenType = expression.getOperationSign();
        if (JSTokenTypes.ANDAND.equals(tokenType) || JSTokenTypes.OROR.equals(tokenType)) {
            return false;
        }
        final JSExpression rhs = expression.getROperand();
        return expressionDefinitelyRecurses(rhs, function);
    }

    private static boolean arrayInitializerExpressionDefinitelyRecurses(JSArrayLiteralExpression expression, JSFunction function) {
        final JSExpression[] initializers = expression.getExpressions();
        for (final JSExpression initializer : initializers) {
            if (expressionDefinitelyRecurses(initializer, function)) {
                return true;
            }
        }
        return false;
    }

    private static boolean prefixExpressionDefinitelyRecurses(JSPrefixExpression expression, JSFunction function) {
        final JSExpression operand = expression.getExpression();
        return expressionDefinitelyRecurses(operand, function);
    }

    private static boolean postfixExpressionDefinitelyRecurses(JSPostfixExpression expression, JSFunction function) {
        final JSExpression operand = expression.getExpression();
        return expressionDefinitelyRecurses(operand, function);
    }

    private static boolean parenthesizedExpressionDefinitelyRecurses(JSParenthesizedExpression expression, JSFunction function) {
        final JSExpression innerExpression = expression.getInnerExpression();
        return expressionDefinitelyRecurses(innerExpression, function);
    }

    private static boolean referenceExpressionDefinitelyRecurses(JSReferenceExpression expression, JSFunction function) {
        final JSExpression qualifierExpression = expression.getQualifier();
        return qualifierExpression != null && expressionDefinitelyRecurses(qualifierExpression, function);
    }

    private static boolean assignmentExpressionDefinitelyRecurses(JSAssignmentExpression assignmentExpression, JSFunction function) {
        final JSExpression rhs = assignmentExpression.getROperand();
        final JSExpression lhs = assignmentExpression.getLOperand();
        return expressionDefinitelyRecurses(rhs, function) || expressionDefinitelyRecurses(lhs, function);
    }


    private static boolean functionCallExpressionDefinitelyRecurses(JSCallExpression exp, JSFunction function) {
        final JSExpression functionExpression = exp.getMethodExpression();
        if (functionExpression instanceof JSReferenceExpression reference) {
            final PsiElement referent = reference.resolve();
            if (referent != null && referent.equals(function)) {
                return true;
            }
            final JSExpression qualifier = reference.getQualifier();
            if (qualifier == null || qualifier instanceof JSThisExpression) {
                return reference.getText().equals(function.getName());
            }
        }

        if (expressionDefinitelyRecurses(functionExpression, function)) {
            return true;
        }
        final JSArgumentList argumentList = exp.getArgumentList();
        final JSExpression[] args = argumentList != null ? argumentList.getArguments() : JSExpression.EMPTY_ARRAY;
        for (final JSExpression arg : args) {
            if (expressionDefinitelyRecurses(arg, function)) {
                return true;
            }
        }
        return false;
    }

    private static boolean statementDefinitelyRecurses(JSStatement statement, JSFunction function) {
        if (statement == null) {
            return false;
        }
        if (statement instanceof JSBreakStatement ||
            statement instanceof JSContinueStatement ||
            statement instanceof JSThrowStatement ||
            statement instanceof JSEmptyStatement) {
            return false;
        }
        else if (statement instanceof JSExpressionStatement expressionStatement) {
            final JSExpression expression = expressionStatement.getExpression();
            return expressionDefinitelyRecurses(expression, function);
        }
        else if (statement instanceof JSVarStatement declaration) {
            final JSVariable[] declaredElements = declaration.getVariables();
            for (final JSVariable variable : declaredElements) {
                final JSExpression initializer = variable.getInitializer();
                if (expressionDefinitelyRecurses(initializer, function)) {
                    return true;
                }
            }
            return false;
        }
        else if (statement instanceof JSReturnStatement returnStatement) {
            final JSExpression returnValue = returnStatement.getExpression();
            return returnValue != null && expressionDefinitelyRecurses(returnValue, function);
        }
        else if (statement instanceof JSForStatement forStatement) {
            return forStatementDefinitelyRecurses(forStatement, function);
        }
        else if (statement instanceof JSForInStatement forInStatement) {
            return foreachStatementDefinitelyRecurses(forInStatement, function);
        }
        else if (statement instanceof JSWhileStatement whileStatement) {
            return whileStatementDefinitelyRecurses(whileStatement, function);
        }
        else if (statement instanceof JSDoWhileStatement doWhileStatement) {
            return doWhileStatementDefinitelyRecurses(doWhileStatement, function);
        }
        else if (statement instanceof JSBlockStatement blockStatement) {
            return codeBlockDefinitelyRecurses(blockStatement, function);
        }
        else if (statement instanceof JSLabeledStatement labeledStatement) {
            return labeledStatementDefinitelyRecurses(labeledStatement, function);
        }
        else if (statement instanceof JSIfStatement ifStatement) {
            return ifStatementDefinitelyRecurses(ifStatement, function);
        }
        else if (statement instanceof JSTryStatement tryStatement) {
            return tryStatementDefinitelyRecurses(tryStatement, function);
        }
        else if (statement instanceof JSSwitchStatement switchStatement) {
            return switchStatementDefinitelyRecurses(switchStatement, function);
        }
        else {
            // unknown statement type
            return false;
        }
    }

    private static boolean switchStatementDefinitelyRecurses(JSSwitchStatement switchStatement, JSFunction function) {
        final JSExpression switchExpression = switchStatement.getSwitchExpression();
        return expressionDefinitelyRecurses(switchExpression, function);
    }

    private static boolean tryStatementDefinitelyRecurses(JSTryStatement tryStatement, JSFunction function) {
        final JSStatement tryBlock = tryStatement.getStatement();
        if (statementDefinitelyRecurses(tryBlock, function)) {
            return true;
        }
        final JSStatement finallyBlock = tryStatement.getFinallyStatement();
        return statementDefinitelyRecurses(finallyBlock, function);
    }

    private static boolean codeBlockDefinitelyRecurses(JSBlockStatement block, JSFunction function) {
        if (block == null) {
            return false;
        }
        final JSStatement[] statements = block.getStatements();
        for (final JSStatement statement : statements) {
            if (statementDefinitelyRecurses(statement, function)) {
                return true;
            }
            else if (statementMayReturnBeforeRecursing(statement, function)) {
                return false;
            }
        }
        return false;
    }

    private static boolean ifStatementDefinitelyRecurses(JSIfStatement ifStatement, JSFunction function) {
        final JSExpression condition = ifStatement.getCondition();
        if (expressionDefinitelyRecurses(condition, function)) {
            return true;
        }
        final JSStatement thenBranch = ifStatement.getThen();
        final JSStatement elseBranch = ifStatement.getElse();
        return thenBranch != null && elseBranch != null
            && statementDefinitelyRecurses(thenBranch, function)
            && statementDefinitelyRecurses(elseBranch, function);
    }

    private static boolean forStatementDefinitelyRecurses(JSForStatement forStatement, JSFunction function) {
        final JSExpression initialization = forStatement.getInitialization();
        if (expressionDefinitelyRecurses(initialization, function)) {
            return true;
        }
        final JSExpression condition = forStatement.getCondition();
        if (expressionDefinitelyRecurses(condition, function)) {
            return true;
        }
        if (BoolUtils.isTrue(condition)) {
            final JSStatement body = forStatement.getBody();
            return statementDefinitelyRecurses(body, function);
        }
        return false;
    }

    private static boolean foreachStatementDefinitelyRecurses(JSForInStatement foreachStatement, JSFunction function) {
        final JSExpression iteration = foreachStatement.getCollectionExpression();
        return expressionDefinitelyRecurses(iteration, function);
    }

    private static boolean whileStatementDefinitelyRecurses(JSWhileStatement whileStatement, JSFunction function) {
        final JSExpression condition = whileStatement.getCondition();
        if (expressionDefinitelyRecurses(condition, function)) {
            return true;
        }
        if (BoolUtils.isTrue(condition)) {
            final JSStatement body = whileStatement.getBody();
            return statementDefinitelyRecurses(body, function);
        }
        return false;
    }

    private static boolean doWhileStatementDefinitelyRecurses(JSDoWhileStatement doWhileStatement, JSFunction function) {
        final JSStatement body = doWhileStatement.getBody();
        if (statementDefinitelyRecurses(body, function)) {
            return true;
        }
        final JSExpression condition = doWhileStatement.getCondition();
        return expressionDefinitelyRecurses(condition, function);
    }

    private static boolean labeledStatementDefinitelyRecurses(JSLabeledStatement labeledStatement, JSFunction function) {
        final JSStatement body = labeledStatement.getStatement();
        return statementDefinitelyRecurses(body, function);
    }

    public static boolean functionDefinitelyRecurses(@Nonnull JSFunction function) {
        final JSSourceElement[] body = function.getBody();
        if (body == null) {
            return false;
        }
        for (JSSourceElement jsSourceElement : body) {
            if (jsSourceElement instanceof JSStatement statement && statementDefinitelyRecurses(statement, function)) {
                return true;
            }
        }
        return false;
    }
}
