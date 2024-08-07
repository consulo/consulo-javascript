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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.language.ast.IElementType;
import jakarta.annotation.Nonnull;

public class RecursionUtil {
    private RecursionUtil() {
    }

    public static boolean statementMayReturnBeforeRecursing(JSStatement statement, JSFunction method) {
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
            return returnValue == null || !RecursionUtil.expressionDefinitelyRecurses(returnValue, method);
        }
        else if (statement instanceof JSForStatement forStatement) {
            return RecursionUtil.forStatementMayReturnBeforeRecursing(forStatement, method);
        }
        else if (statement instanceof JSForInStatement forInStatement) {
            return RecursionUtil.forInStatementMayReturnBeforeRecursing(forInStatement, method);
        }
        else if (statement instanceof JSWhileStatement whileStatement) {
            return RecursionUtil.whileStatementMayReturnBeforeRecursing(whileStatement, method);
        }
        else if (statement instanceof JSDoWhileStatement doWhileStatement) {
            return RecursionUtil.doWhileStatementMayReturnBeforeRecursing(doWhileStatement, method);
        }
        else if (statement instanceof JSBlockStatement blockStatement) {
            return RecursionUtil.blockStatementMayReturnBeforeRecursing(blockStatement, method, false);
        }
        else if (statement instanceof JSLabeledStatement labeledStatement) {
            return RecursionUtil.labeledStatementMayReturnBeforeRecursing(labeledStatement, method);
        }
        else if (statement instanceof JSIfStatement ifStatement) {
            return RecursionUtil.ifStatementMayReturnBeforeRecursing(ifStatement, method);
        }
        else if (statement instanceof JSTryStatement tryStatement) {
            return RecursionUtil.tryStatementMayReturnBeforeRecursing(tryStatement, method);
        }
        else if (statement instanceof JSSwitchStatement switchStatement) {
            return RecursionUtil.switchStatementMayReturnBeforeRecursing(switchStatement, method);
        }
        else {
            // unknown statement type
            return true;
        }
    }

    private static boolean doWhileStatementMayReturnBeforeRecursing(JSDoWhileStatement loopStatement, JSFunction method) {
        final JSStatement body = loopStatement.getBody();
        return RecursionUtil.statementMayReturnBeforeRecursing(body, method);
    }

    private static boolean whileStatementMayReturnBeforeRecursing(JSWhileStatement loopStatement, JSFunction method) {
        final JSExpression test = loopStatement.getCondition();
        if (RecursionUtil.expressionDefinitelyRecurses(test, method)) {
            return false;
        }
        final JSStatement body = loopStatement.getBody();
        return RecursionUtil.statementMayReturnBeforeRecursing(body, method);
    }

    private static boolean forStatementMayReturnBeforeRecursing(JSForStatement loopStatement, JSFunction method) {
        if (RecursionUtil.statementMayReturnBeforeRecursing(loopStatement.getVarDeclaration(), method)) {
            return true;
        }
        if (RecursionUtil.expressionDefinitelyRecurses(loopStatement.getInitialization(), method)) {
            return false;
        }
        if (RecursionUtil.expressionDefinitelyRecurses(loopStatement.getCondition(), method)) {
            return false;
        }
        return RecursionUtil.statementMayReturnBeforeRecursing(loopStatement.getBody(), method);
    }

    private static boolean forInStatementMayReturnBeforeRecursing(JSForInStatement loopStatement, JSFunction method) {
        final JSExpression collection = loopStatement.getCollectionExpression();
        if (RecursionUtil.expressionDefinitelyRecurses(collection, method)) {
            return false;
        }
        final JSStatement body = loopStatement.getBody();
        return RecursionUtil.statementMayReturnBeforeRecursing(body, method);
    }

    private static boolean switchStatementMayReturnBeforeRecursing(JSSwitchStatement switchStatement, JSFunction method) {
        for (final JSCaseClause clause : switchStatement.getCaseClauses()) {
            for (final JSStatement statement : clause.getStatements()) {
                if (RecursionUtil.statementMayReturnBeforeRecursing(statement, method)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static boolean tryStatementMayReturnBeforeRecursing(JSTryStatement tryStatement, JSFunction method) {
        final JSStatement finallyStatement = tryStatement.getFinallyStatement();

        if (finallyStatement != null) {
            if (RecursionUtil.statementMayReturnBeforeRecursing(finallyStatement, method)) {
                return true;
            }
            if (RecursionUtil.statementDefinitelyRecurses(finallyStatement, method)) {
                return false;
            }
        }

        final JSStatement statement = tryStatement.getStatement();

        if (RecursionUtil.statementMayReturnBeforeRecursing(statement, method)) {
            return true;
        }

        final JSCatchBlock catchBlock = tryStatement.getCatchBlock();

        if (catchBlock != null) {
            final JSStatement catchStatement = catchBlock.getStatement();

            if (RecursionUtil.statementMayReturnBeforeRecursing(catchStatement, method)) {
                return true;
            }
        }
        return false;
    }

    private static boolean ifStatementMayReturnBeforeRecursing(JSIfStatement ifStatement, JSFunction method) {
        final JSExpression test = ifStatement.getCondition();
        if (RecursionUtil.expressionDefinitelyRecurses(test, method)) {
            return false;
        }
        final JSStatement thenBranch = ifStatement.getThen();
        if (RecursionUtil.statementMayReturnBeforeRecursing(thenBranch, method)) {
            return true;
        }
        final JSStatement elseBranch = ifStatement.getElse();
        return elseBranch != null && RecursionUtil.statementMayReturnBeforeRecursing(elseBranch, method);
    }

    private static boolean labeledStatementMayReturnBeforeRecursing(JSLabeledStatement labeledStatement, JSFunction method) {
        final JSStatement statement = labeledStatement.getStatement();
        return RecursionUtil.statementMayReturnBeforeRecursing(statement, method);
    }

    private static boolean blockStatementMayReturnBeforeRecursing(JSBlockStatement block, JSFunction method, boolean endsInImplicitReturn) {
        if (block == null) {
            return true;
        }

        final JSStatement[] statements = block.getStatements();

        for (final JSStatement statement : statements) {
            if (RecursionUtil.statementMayReturnBeforeRecursing(statement, method)) {
                return true;
            }
            if (RecursionUtil.statementDefinitelyRecurses(statement, method)) {
                return false;
            }
        }
        return endsInImplicitReturn;
    }

    public static boolean functionMayRecurse(@Nonnull JSFunction function) {
        final JSRecursionVisitor recursionVisitor = new JSRecursionVisitor(function);

        if (recursionVisitor.isFunctionNamed()) {
            function.accept(recursionVisitor);
        }
        return recursionVisitor.isRecursive();
    }

    private static boolean expressionDefinitelyRecurses(JSExpression exp, JSFunction method) {
        if (exp == null) {
            return false;
        }
        if (exp instanceof JSNewExpression newExpression) {
            return RecursionUtil.newExpressionDefinitelyRecurses(newExpression, method);
        }
        if (exp instanceof JSCallExpression callExpression) {
            return RecursionUtil.callExpressionDefinitelyRecurses(callExpression, method);
        }
        if (exp instanceof JSAssignmentExpression assignmentExpression) {
            return RecursionUtil.assignmentExpressionDefinitelyRecurses(assignmentExpression, method);
        }
        if (exp instanceof JSArrayLiteralExpression arrayLiteralExpression) {
            return RecursionUtil.arrayLiteralExpressionDefinitelyRecurses(arrayLiteralExpression, method);
        }
        if (exp instanceof JSIndexedPropertyAccessExpression indexedPropertyAccessExpression) {
            return RecursionUtil.indexedPropertyAccessExpressionDefinitelyRecurses(indexedPropertyAccessExpression, method);
        }
        if (exp instanceof JSPrefixExpression prefixExpression) {
            return RecursionUtil.prefixExpressionDefinitelyRecurses(prefixExpression, method);
        }
        if (exp instanceof JSPostfixExpression postfixExpression) {
            return RecursionUtil.postfixExpressionDefinitelyRecurses(postfixExpression, method);
        }
        if (exp instanceof JSBinaryExpression binaryExpression) {
            return RecursionUtil.binaryExpressionDefinitelyRecurses(binaryExpression, method);
        }
        if (exp instanceof JSConditionalExpression conditionalExpression) {
            return RecursionUtil.conditionalExpressionDefinitelyRecurses(conditionalExpression, method);
        }
        if (exp instanceof JSParenthesizedExpression parenthesizedExpression) {
            return RecursionUtil.parenthesizedExpressionDefinitelyRecurses(parenthesizedExpression, method);
        }
        if (exp instanceof JSReferenceExpression referenceExpression) {
            return RecursionUtil.referenceExpressionDefinitelyRecurses(referenceExpression, method);
        }
        if (exp instanceof JSLiteralExpression || exp instanceof JSThisExpression) {
            return false;
        }
        return false;
    }

    private static boolean conditionalExpressionDefinitelyRecurses(JSConditionalExpression expression, JSFunction method) {
        final JSExpression condExpression = expression.getCondition();

        return RecursionUtil.expressionDefinitelyRecurses(condExpression, method) ||
            (RecursionUtil.expressionDefinitelyRecurses(expression.getThen(), method) &&
                RecursionUtil.expressionDefinitelyRecurses(expression.getElse(), method));
    }

    private static boolean binaryExpressionDefinitelyRecurses(JSBinaryExpression expression, JSFunction method) {
        final JSExpression lhs = expression.getLOperand();

        if (RecursionUtil.expressionDefinitelyRecurses(lhs, method)) {
            return true;
        }

        final IElementType tokenType = expression.getOperationSign();

        if (JSTokenTypes.ANDAND.equals(tokenType) || JSTokenTypes.OROR.equals(tokenType)) {
            return false;
        }

        return RecursionUtil.expressionDefinitelyRecurses(expression.getROperand(), method);
    }

    private static boolean indexedPropertyAccessExpressionDefinitelyRecurses(
        JSIndexedPropertyAccessExpression expression,
        JSFunction method
    ) {
        final JSExpression arrayExp = expression.getQualifier();
        final JSExpression indexExp = expression.getIndexExpression();
        return RecursionUtil.expressionDefinitelyRecurses(arrayExp, method)
            || RecursionUtil.expressionDefinitelyRecurses(indexExp, method);
    }

    private static boolean arrayLiteralExpressionDefinitelyRecurses(JSArrayLiteralExpression expression, JSFunction method) {
        for (final JSExpression initializer : expression.getExpressions()) {
            if (RecursionUtil.expressionDefinitelyRecurses(initializer, method)) {
                return true;
            }
        }
        return false;
    }

    private static boolean prefixExpressionDefinitelyRecurses(JSPrefixExpression expression, JSFunction method) {
        final JSExpression operand = expression.getExpression();
        return RecursionUtil.expressionDefinitelyRecurses(operand, method);
    }

    private static boolean postfixExpressionDefinitelyRecurses(JSPostfixExpression expression, JSFunction method) {
        final JSExpression operand = expression.getExpression();
        return RecursionUtil.expressionDefinitelyRecurses(operand, method);
    }

    private static boolean parenthesizedExpressionDefinitelyRecurses(JSParenthesizedExpression expression, JSFunction method) {
        final JSExpression innerExpression = expression.getInnerExpression();
        return RecursionUtil.expressionDefinitelyRecurses(innerExpression, method);
    }

    private static boolean referenceExpressionDefinitelyRecurses(JSReferenceExpression expression, JSFunction method) {
        final JSExpression qualifierExpression = expression.getQualifier();
        return (qualifierExpression != null &&
            RecursionUtil.expressionDefinitelyRecurses(qualifierExpression, method));
    }

    private static boolean assignmentExpressionDefinitelyRecurses(JSAssignmentExpression assignmentExpression, JSFunction method) {
        final JSExpression lhs = assignmentExpression.getLOperand();
        final JSExpression rhs = assignmentExpression.getROperand();
        return RecursionUtil.expressionDefinitelyRecurses(rhs, method)
            || RecursionUtil.expressionDefinitelyRecurses(lhs, method);
    }

    private static boolean newExpressionDefinitelyRecurses(JSNewExpression exp, JSFunction method) {
        final JSArgumentList argumentList = exp.getArgumentList();
        if (argumentList != null) {
            final JSExpression[] args = argumentList.getArguments();
            for (final JSExpression arg : args) {
                if (RecursionUtil.expressionDefinitelyRecurses(arg, method)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static boolean callExpressionDefinitelyRecurses(JSCallExpression exp, JSFunction method) {
        final JSFunction calledMethod = ControlFlowUtils.resolveMethod(exp);

        if (calledMethod != null && calledMethod.equals(method)) {
            return true;
        }

        final JSExpression methodExpression = exp.getMethodExpression();

        if (methodExpression == null) {
            return false;
        }
        else if (RecursionUtil.expressionDefinitelyRecurses(methodExpression, method)) {
            return true;
        }

        for (final JSExpression arg : exp.getArgumentList().getArguments()) {
            if (RecursionUtil.expressionDefinitelyRecurses(arg, method)) {
                return true;
            }
        }
        return false;
    }

    private static boolean statementDefinitelyRecurses(JSStatement statement, JSFunction method) {
        if (statement == null) {
            return false;
        }
        if (statement instanceof JSBreakStatement
            || statement instanceof JSContinueStatement
            || statement instanceof JSThrowStatement
            || statement instanceof JSEmptyStatement) {
            return false;
        }
        else if (statement instanceof JSExpressionStatement expressionStatement) {
            final JSExpression expression = expressionStatement.getExpression();
            return RecursionUtil.expressionDefinitelyRecurses(expression, method);
        }
        else if (statement instanceof JSVarStatement varStatement) {
            for (final JSVariable variable : varStatement.getVariables()) {
                final JSExpression initializer = variable.getInitializer();
                if (RecursionUtil.expressionDefinitelyRecurses(initializer, method)) {
                    return true;
                }
            }
            return false;
        }
        else if (statement instanceof JSReturnStatement returnStatement) {
            final JSExpression returnValue = returnStatement.getExpression();
            if (returnValue != null) {
                if (RecursionUtil.expressionDefinitelyRecurses(returnValue, method)) {
                    return true;
                }
            }
            return false;
        }
        else if (statement instanceof JSForStatement forStatement) {
            return RecursionUtil.forStatementDefinitelyRecurses(forStatement, method);
        }
        else if (statement instanceof JSForInStatement forInStatement) {
            return RecursionUtil.forInStatementDefinitelyRecurses(forInStatement, method);
        }
        else if (statement instanceof JSWhileStatement whileStatement) {
            return RecursionUtil.whileStatementDefinitelyRecurses(whileStatement, method);
        }
        else if (statement instanceof JSDoWhileStatement doWhileStatement) {
            return RecursionUtil.doWhileStatementDefinitelyRecurses(doWhileStatement, method);
        }
        else if (statement instanceof JSBlockStatement blockStatement) {
            return RecursionUtil.blockStatementDefinitelyRecurses(blockStatement, method);
        }
        else if (statement instanceof JSLabeledStatement labeledStatement) {
            return RecursionUtil.labeledStatementDefinitelyRecurses(labeledStatement, method);
        }
        else if (statement instanceof JSIfStatement ifStatement) {
            return RecursionUtil.ifStatementDefinitelyRecurses(ifStatement, method);
        }
        else if (statement instanceof JSTryStatement tryStatement) {
            return RecursionUtil.tryStatementDefinitelyRecurses(tryStatement, method);
        }
        else if (statement instanceof JSSwitchStatement switchStatement) {
            return RecursionUtil.switchStatementDefinitelyRecurses(switchStatement, method);
        }
        else {
            // unknown statement type
            return false;
        }
    }

    private static boolean switchStatementDefinitelyRecurses(JSSwitchStatement switchStatement, JSFunction method) {
        final JSExpression switchExpression = switchStatement.getSwitchExpression();
        return RecursionUtil.expressionDefinitelyRecurses(switchExpression, method);
    }

    private static boolean tryStatementDefinitelyRecurses(JSTryStatement tryStatement, JSFunction method) {
        final JSStatement statement = tryStatement.getStatement();
        if (RecursionUtil.statementDefinitelyRecurses(statement, method)) {
            return true;
        }
        final JSStatement finallyStatement = tryStatement.getFinallyStatement();
        return RecursionUtil.statementDefinitelyRecurses(finallyStatement, method);
    }

    private static boolean blockStatementDefinitelyRecurses(JSBlockStatement block, JSFunction method) {
        if (block != null) {
            for (final JSStatement statement : block.getStatements()) {
                if (RecursionUtil.statementDefinitelyRecurses(statement, method)) {
                    return true;
                }
            }
        }
        return false;
    }

    private static boolean ifStatementDefinitelyRecurses(JSIfStatement ifStatement, JSFunction method) {
        final JSExpression condition = ifStatement.getCondition();
        if (RecursionUtil.expressionDefinitelyRecurses(condition, method)) {
            return true;
        }
        final JSStatement thenBranch = ifStatement.getThen();
        final JSStatement elseBranch = ifStatement.getElse();
        if (thenBranch == null || elseBranch == null) {
            return false;
        }
        return RecursionUtil.statementDefinitelyRecurses(thenBranch, method)
            && RecursionUtil.statementDefinitelyRecurses(elseBranch, method);
    }

    private static boolean forStatementDefinitelyRecurses(JSForStatement forStatement, JSFunction method) {
        final JSStatement declaration = forStatement.getVarDeclaration();
        final JSExpression initialization = forStatement.getInitialization();

        if (declaration != null &&
            RecursionUtil.statementDefinitelyRecurses(declaration, method)) {
            return true;
        }
        else if (initialization != null &&
            RecursionUtil.expressionDefinitelyRecurses(initialization, method)) {
            return true;
        }

        final JSExpression condition = forStatement.getCondition();

        if (RecursionUtil.expressionDefinitelyRecurses(condition, method)) {
            return true;
        }
        if (BoolUtils.isTrue(condition)) {
            final JSStatement body = forStatement.getBody();
            return RecursionUtil.statementDefinitelyRecurses(body, method);
        }
        return false;
    }

    private static boolean forInStatementDefinitelyRecurses(JSForInStatement foreachStatement, JSFunction method) {
        final JSExpression collection = foreachStatement.getCollectionExpression();
        return RecursionUtil.expressionDefinitelyRecurses(collection, method);
    }

    private static boolean whileStatementDefinitelyRecurses(JSWhileStatement whileStatement, JSFunction method) {
        final JSExpression condition = whileStatement.getCondition();
        if (RecursionUtil.expressionDefinitelyRecurses(condition, method)) {
            return true;
        }
        if (BoolUtils.isTrue(condition)) {
            final JSStatement body = whileStatement.getBody();
            return RecursionUtil.statementDefinitelyRecurses(body, method);
        }
        return false;
    }

    private static boolean doWhileStatementDefinitelyRecurses(JSDoWhileStatement doWhileStatement, JSFunction method) {
        final JSStatement body = doWhileStatement.getBody();
        if (RecursionUtil.statementDefinitelyRecurses(body, method)) {
            return true;
        }
        final JSExpression condition = doWhileStatement.getCondition();
        return RecursionUtil.expressionDefinitelyRecurses(condition, method);
    }

    private static boolean labeledStatementDefinitelyRecurses(JSLabeledStatement labeledStatement, JSFunction method) {
        final JSStatement body = labeledStatement.getStatement();
        return RecursionUtil.statementDefinitelyRecurses(body, method);
    }

    public static boolean functionDefinitelyRecurses(@Nonnull JSFunction method) {
        final JSSourceElement[] body = method.getBody();

        if (body != null) {
            for (final JSSourceElement element : body) {
                if (element instanceof JSStatement &&
                    RecursionUtil.statementDefinitelyRecurses((JSStatement)element, method)) {
                    return true;
                }
                else if (element instanceof JSExpression &&
                    RecursionUtil.expressionDefinitelyRecurses((JSExpression)element, method)) {
                    return true;
                }
            }
        }
        return false;
    }
}
