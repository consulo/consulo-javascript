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

import consulo.javascript.language.psi.JavaScriptType;
import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.ast.IElementType;

public class EquivalenceChecker {

    private EquivalenceChecker() {}

    private static final int THIS_EXPRESSION                  =  0;
    private static final int LITERAL_EXPRESSION               =  1;
    private static final int CLASS_OBJECT_EXPRESSION          =  2;
    private static final int REFERENCE_EXPRESSION             =  3;
    private static final int SUPER_EXPRESSION                 =  4;
    private static final int CALL_EXPRESSION                  =  5;
    private static final int NEW_EXPRESSION                   =  6;
    private static final int ARRAY_LITERAL_EXPRESSION         =  7;
    private static final int PREFIX_EXPRESSION                =  8;
    private static final int POSTFIX_EXPRESSION               =  9;
    private static final int ASSIGNMENT_EXPRESSION            = 10;
    private static final int BINARY_EXPRESSION                = 11;
    private static final int CONDITIONAL_EXPRESSION           = 12;
    private static final int INDEX_PROPERTY_ACCESS_EXPRESSION = 13;
    private static final int OBJECT_LITERAL_EXPRESSION        = 14;
    private static final int FUNCTION_EXPRESSION              = 15;
    private static final int DEFINITION_EXPRESSION            = 16;

    private static final int BLOCK_STATEMENT                  =  0;
    private static final int BREAK_STATEMENT                  =  1;
    private static final int CONTINUE_STATEMENT               =  2;
    private static final int VAR_STATEMENT                    =  3;
    private static final int DO_WHILE_STATEMENT               =  4;
    private static final int EMPTY_STATEMENT                  =  5;
    private static final int EXPRESSION_STATEMENT             =  6;
    private static final int FOR_STATEMENT                    =  7;
    private static final int FOR_IN_STATEMENT                 =  8;
    private static final int IF_STATEMENT                     =  9;
    private static final int LABELED_STATEMENT                = 10;
    private static final int RETURN_STATEMENT                 = 11;
    private static final int SWITCH_STATEMENT                 = 12;
    private static final int THROW_STATEMENT                  = 13;
    private static final int TRY_STATEMENT                    = 14;
    private static final int WHILE_STATEMENT                  = 15;
    private static final int WITH_STATEMENT                   = 16;

    public static boolean statementsAreEquivalent(JSStatement exp1,
                                                  JSStatement exp2) {
        if (exp1 == null && exp2 == null) {
            return true;
        }

        if (exp1 == null || exp2 == null) {
            return false;
        }

        final int type1 = getStatementType(exp1);
        final int type2 = getStatementType(exp2);

        if (type1 != type2) {
            return false;
        }

        switch (type1) {
            case BLOCK_STATEMENT:
                return codeBlocksAreEquivalent((JSBlockStatement) exp1,
                                               (JSBlockStatement) exp2);
            case BREAK_STATEMENT:
                return breakStatementsAreEquivalent((JSBreakStatement) exp1,
                                                    (JSBreakStatement) exp2);
            case CONTINUE_STATEMENT:
                return continueStatementsAreEquivalent((JSContinueStatement) exp1,
                                                       (JSContinueStatement) exp2);
            case VAR_STATEMENT:
                return declarationStatementsAreEquivalent((JSVarStatement) exp1,
                                                          (JSVarStatement) exp2);
            case DO_WHILE_STATEMENT:
                return doWhileStatementsAreEquivalent((JSDoWhileStatement) exp1,
                                                      (JSDoWhileStatement) exp2);
            case EMPTY_STATEMENT:
                return true;
            case EXPRESSION_STATEMENT:
                return expressionStatementsAreEquivalent((JSExpressionStatement) exp1,
                                                         (JSExpressionStatement) exp2);
            case FOR_STATEMENT:
                return forStatementsAreEquivalent((JSForStatement) exp1,
                                                  (JSForStatement) exp2);
            case FOR_IN_STATEMENT:
                return forInStatementsAreEquivalent((JSForInStatement) exp1,
                                                    (JSForInStatement) exp2);
            case IF_STATEMENT:
                return ifStatementsAreEquivalent((JSIfStatement) exp1,
                                                 (JSIfStatement) exp2);
            case LABELED_STATEMENT:
                return labeledStatementsAreEquivalent((JSLabeledStatement) exp1,
                                                      (JSLabeledStatement) exp2);
            case RETURN_STATEMENT:
                return returnStatementsAreEquivalent((JSReturnStatement) exp1,
                                                     (JSReturnStatement) exp2);
            case SWITCH_STATEMENT:
                return switchStatementsAreEquivalent((JSSwitchStatement) exp1,
                                                     (JSSwitchStatement) exp2);
            case THROW_STATEMENT:
                return throwStatementsAreEquivalent((JSThrowStatement) exp1,
                                                    (JSThrowStatement) exp2);
            case TRY_STATEMENT:
                return tryStatementsAreEquivalent((JSTryStatement) exp1,
                                                  (JSTryStatement) exp2);
            case WHILE_STATEMENT:
                return whileStatementsAreEquivalent((JSWhileStatement) exp1,
                                                    (JSWhileStatement) exp2);
            case WITH_STATEMENT:
                return withStatementsAreEquivalent((JSWithStatement) exp1,
                                                   (JSWithStatement) exp2);
            default:
                return false;
        }
    }

    private static boolean declarationStatementsAreEquivalent(JSVarStatement statement1,
                                                              JSVarStatement statement2) {
        if (statement1 == null || statement2 == null) return statement1 == statement2;
        JSVariable[] vars1 = statement1.getVariables();
        JSVariable[] vars2 = statement2.getVariables();

        if (vars1.length == vars2.length) {
            for (int index = 0; index < vars2.length; index++) {
                if (!localVariableAreEquivalent(vars1[index], vars2[index])) {
                    return false;
                }
            }
        }

        return true;
    }

    private static boolean tryStatementsAreEquivalent(JSTryStatement statement1,
                                                      JSTryStatement statement2) {
        if (!statementsAreEquivalent(statement1.getStatement(),
                                     statement2.getStatement())) {
            return false;
        }
        if (!statementsAreEquivalent(statement1.getFinallyStatement(),
                                     statement2.getFinallyStatement())) {
            return false;
        }
        JSCatchBlock catchBlock1 = statement1.getCatchBlock();
        JSCatchBlock catchBlock2 = statement2.getCatchBlock();
        if (catchBlock1 == null) {
            return (catchBlock2 == null);
        } else if (catchBlock2 == null) {
            return false;
        }

        if (!parametersAreEquivalent(catchBlock1.getParameter(),
                                     catchBlock2.getParameter())) {
            return false;
        }
        return statementsAreEquivalent(catchBlock1.getStatement(),
                                       catchBlock2.getStatement());
    }

    private static boolean localVariableAreEquivalent(JSVariable var1,
                                                      JSVariable var2) {
        return (expressionsAreEquivalent(var1.getInitializer(), var2.getInitializer()) &&
                typesAreEquivalent      (var1.getType(),        var2.getType())        &&
                areEqual                (var1.getName(),        var2.getName()));
    }

    private static boolean parametersAreEquivalent(JSParameter parameter1,
                                                   JSParameter parameter2) {
        return (typesAreEquivalent(parameter1.getType(), parameter2.getType())        &&
                areEqual          (parameter1.getName(), parameter2.getName()));
    }

    private static boolean typesAreEquivalent(JavaScriptType type1, JavaScriptType type2) {
        return areEqual(type1, type2);
    }

    private static boolean whileStatementsAreEquivalent(JSWhileStatement statement1,
                                                        JSWhileStatement statement2) {
        return (expressionsAreEquivalent(statement1.getCondition(), statement2.getCondition()) &&
                statementsAreEquivalent (statement1.getBody(),      statement2.getBody()));
    }

    private static boolean forStatementsAreEquivalent(JSForStatement statement1,
                                                      JSForStatement statement2) {
        return (expressionsAreEquivalent(statement1.getCondition(),      statement2.getCondition())      &&
                expressionsAreEquivalent(statement1.getInitialization(), statement2.getInitialization()) &&
                expressionsAreEquivalent(statement1.getUpdate(),         statement2.getUpdate())         &&
                statementsAreEquivalent (statement1.getBody(),           statement2.getBody()));
    }

    private static boolean forInStatementsAreEquivalent(JSForInStatement statement1,
                                                        JSForInStatement statement2) {
        return (expressionsAreEquivalent          (statement1.getCollectionExpression(), statement2.getCollectionExpression()) &&
                expressionsAreEquivalent          (statement1.getVariableExpression(), statement2.getVariableExpression()) &&
                declarationStatementsAreEquivalent(statement1.getDeclarationStatement(), statement2.getDeclarationStatement()) &&
                statementsAreEquivalent           (statement1.getBody(),                 statement2.getBody()));
    }

    private static boolean switchStatementsAreEquivalent(JSSwitchStatement statement1,
                                                         JSSwitchStatement statement2) {
        if (!expressionsAreEquivalent(statement1.getSwitchExpression(),
                                      statement2.getSwitchExpression())) {
            return false;
        }

        final JSCaseClause[] caseClauses1 = statement1.getCaseClauses();
        final JSCaseClause[] caseClauses2 = statement2.getCaseClauses();

        if (caseClauses1.length != caseClauses2.length) {
            return false;
        }

        for (int index = 0; index < caseClauses1.length; index++) {
           if (!caseClausesAreEquivalent(caseClauses1[index], caseClauses2[index])) {
                return false;
            }
        }

        return true;
    }

    private static boolean caseClausesAreEquivalent(JSCaseClause caseClause1, JSCaseClause caseClause2) {
        if (caseClause1.isDefault() != caseClause2.isDefault()) {
            return false;
        }
        if (!expressionsAreEquivalent(caseClause1.getCaseExpression(), caseClause2.getCaseExpression())) {
            return false;
        }

        final JSStatement[] statements1 = caseClause1.getStatements();
        final JSStatement[] statements2 = caseClause2.getStatements();

        if (statements1.length != statements2.length) {
            return false;
        }

        for (int index = 0; index < statements1.length; index++) {
            if (!statementsAreEquivalent(statements1[index], statements2[index])) {
                return false;
            }
        }

        return false;
    }

    private static boolean doWhileStatementsAreEquivalent(JSDoWhileStatement statement1,
                                                          JSDoWhileStatement statement2) {
        return (expressionsAreEquivalent(statement1.getCondition(), statement2.getCondition()) &&
                statementsAreEquivalent (statement1.getBody(),      statement2.getBody()));
    }

    private static boolean codeBlocksAreEquivalent(JSBlockStatement statement1,
                                                   JSBlockStatement statement2) {
        if (statement1 == null && statement2 == null) {
            return true;
        }
        if (statement1 == null || statement2 == null) {
            return false;
        }
        return blockStatementsAreEquivalent(statement1.getStatements(), statement2.getStatements());
    }

    private static boolean breakStatementsAreEquivalent(JSBreakStatement statement1,
                                                        JSBreakStatement statement2) {
        return areEqual(statement1.getLabel(), statement2.getLabel());
    }

    private static boolean continueStatementsAreEquivalent(JSContinueStatement statement1,
                                                           JSContinueStatement statement2) {
        return areEqual(statement1.getLabel(), statement2.getLabel());
    }

    private static boolean labeledStatementsAreEquivalent(JSLabeledStatement statement1,
                                                          JSLabeledStatement statement2) {
        final PsiElement element1 = statement1.getLabelIdentifier();
        final PsiElement element2 = statement2.getLabelIdentifier();

        return ((element1 == null) ? (element2 == null)
                                   : (element2 != null && element1.getText().equals(element2.getText())));
    }

    private static boolean blockStatementsAreEquivalent(JSStatement[] statements1,
                                                        JSStatement[] statements2) {
        if (statements1.length != statements2.length) {
            return false;
        }
        for (int index = 0; index < statements1.length; index++) {
            if (!statementsAreEquivalent(statements1[index], statements2[index])) {
                return false;
            }
        }
        return true;
    }

    private static boolean ifStatementsAreEquivalent(JSIfStatement statement1,
                                                     JSIfStatement statement2) {
        return (expressionsAreEquivalent(statement1.getCondition(), statement2.getCondition()) &&
                statementsAreEquivalent (statement1.getThen(),      statement2.getThen())      &&
                statementsAreEquivalent (statement1.getElse(),      statement2.getElse()));
    }

    private static boolean expressionStatementsAreEquivalent(JSExpressionStatement statement1,
                                                             JSExpressionStatement statement2) {
        return expressionsAreEquivalent(statement1.getExpression(), statement2.getExpression());
    }

    private static boolean returnStatementsAreEquivalent(JSReturnStatement statement1,
                                                         JSReturnStatement statement2) {
        return expressionsAreEquivalent(statement1.getExpression(), statement2.getExpression());
    }

    private static boolean throwStatementsAreEquivalent(JSThrowStatement statement1,
                                                        JSThrowStatement statement2) {
        return expressionsAreEquivalent(statement1.getExpression(), statement2.getExpression());
    }

    private static boolean withStatementsAreEquivalent(JSWithStatement statement1,
                                                       JSWithStatement statement2) {
        return (expressionsAreEquivalent(statement1.getExpression(), statement2.getExpression()) &&
                statementsAreEquivalent (statement1.getStatement(),  statement2.getStatement()));
    }

    public static boolean expressionsAreEquivalent(JSExpression exp1,
                                                   JSExpression exp2) {
        if (exp1 == null && exp2 == null) {
            return true;
        }
        if (exp1 == null || exp2 == null) {
            return false;
        }

        final JSExpression expToCompare1 = ParenthesesUtils.stripParentheses(exp1);
        final JSExpression expToCompare2 = ParenthesesUtils.stripParentheses(exp2);
        final int          type1         = getExpressionType(expToCompare1);
        final int          type2         = getExpressionType(expToCompare2);

        if (type1 != type2) {
            return false;
        }
        switch(type1) {
            case THIS_EXPRESSION:
            case SUPER_EXPRESSION:
                return true;

            case LITERAL_EXPRESSION:
            case CLASS_OBJECT_EXPRESSION:
            case REFERENCE_EXPRESSION:
            case DEFINITION_EXPRESSION:
                return expToCompare1.getText().equals(expToCompare2.getText());

            case CALL_EXPRESSION:
                return callExpressionsAreEquivalent((JSCallExpression) expToCompare1,
                                                    (JSCallExpression) expToCompare2);

            case NEW_EXPRESSION:
                return newExpressionsAreEquivalent((JSNewExpression) expToCompare1,
                                                   (JSNewExpression) expToCompare2);

            case ARRAY_LITERAL_EXPRESSION:
                return arrayLiteralExpressionsAreEquivalent((JSArrayLiteralExpression) expToCompare1,
                                                            (JSArrayLiteralExpression) expToCompare2);

            case PREFIX_EXPRESSION:
                return prefixExpressionsAreEquivalent((JSPrefixExpression) expToCompare1,
                                                      (JSPrefixExpression) expToCompare2);

            case POSTFIX_EXPRESSION:
                return postfixExpressionsAreEquivalent((JSPostfixExpression) expToCompare1,
                                                       (JSPostfixExpression) expToCompare2);

            case BINARY_EXPRESSION:
                return binaryExpressionsAreEquivalent((JSBinaryExpression) expToCompare1,
                                                      (JSBinaryExpression) expToCompare2);

            case ASSIGNMENT_EXPRESSION:
                return assignmentExpressionsAreEquivalent((JSAssignmentExpression) expToCompare1,
                                                          (JSAssignmentExpression) expToCompare2);

            case CONDITIONAL_EXPRESSION:
                return conditionalExpressionsAreEquivalent((JSConditionalExpression) expToCompare1,
                                                           (JSConditionalExpression) expToCompare2);

            case INDEX_PROPERTY_ACCESS_EXPRESSION:
                return indexedPropertyAccessExpressionsAreEquivalent((JSIndexedPropertyAccessExpression) expToCompare1,
                                                                     (JSIndexedPropertyAccessExpression) expToCompare2);

            case OBJECT_LITERAL_EXPRESSION:
                return objectLiteralExpressionsAreEquivalent((JSObjectLiteralExpression) expToCompare1,
                                                             (JSObjectLiteralExpression) expToCompare2);

            case FUNCTION_EXPRESSION:
                return functionExpressionsAreEquivalent((JSFunctionExpression) expToCompare1,
                                                        (JSFunctionExpression) expToCompare2);

            default:
                return false;
        }
    }

    @SuppressWarnings({"UNUSED_SYMBOL"})
    private static boolean functionExpressionsAreEquivalent(JSFunctionExpression functionExp1,
                                                            JSFunctionExpression functionExp2) {
        return false;
    }

    private static boolean objectLiteralExpressionsAreEquivalent(JSObjectLiteralExpression objectLiteralExp1,
                                                                 JSObjectLiteralExpression objectLiteralExp2) {
        final JSProperty[] properties1 = objectLiteralExp1.getProperties();
        final JSProperty[] properties2 = objectLiteralExp2.getProperties();

        if (properties1.length != properties2.length) {
            return false;
        }

        for (int index = 0; index < properties2.length; index++) {
            final JSProperty property1     = properties1[index];
            final JSProperty property2     = properties2[index];
            final String     propertyName1 = property1.getName();
            final String     propertyName2 = property2.getName();

            if (!(propertyName1 != null && propertyName2 != null &&
                  propertyName2.equals(propertyName1) &&
                  expressionsAreEquivalent(property1.getValue(), property2.getValue()))) {
                return false;
            }
        }

        return true;
    }

    private static boolean indexedPropertyAccessExpressionsAreEquivalent(JSIndexedPropertyAccessExpression exp1,
                                                                         JSIndexedPropertyAccessExpression exp2) {
        return (expressionsAreEquivalent(exp1.getIndexExpression(), exp2.getIndexExpression())  &&
                expressionsAreEquivalent(exp1.getQualifier(),       exp2.getQualifier()));
    }

    private static boolean callExpressionsAreEquivalent(JSCallExpression methodExp1,
                                                        JSCallExpression methodExp2) {
        JSExpression exp1;
        JSExpression exp2;

        try {
            exp1 = methodExp1.getMethodExpression();
            exp2 = methodExp2.getMethodExpression();
        } catch (Exception exception) {
            return false;
        }

        if (!expressionsAreEquivalent(exp1, exp2)) {
            return false;
        }

        final JSArgumentList argumentList1 = methodExp1.getArgumentList();
        final JSArgumentList argumentList2 = methodExp2.getArgumentList();

        return (argumentList1 != null &&
                argumentList2 != null &&
                expressionListsAreEquivalent(argumentList1.getArguments(), argumentList2.getArguments()));
    }

    private static boolean newExpressionsAreEquivalent(JSNewExpression newExp1,
                                                       JSNewExpression newExp2) {
        final JSExpression exp1 = newExp1.getMethodExpression();
        final JSExpression exp2 = newExp2.getMethodExpression();

        if (!expressionsAreEquivalent(exp1, exp2)) {
            return false;
        }

        final JSArgumentList argumentList1 = newExp1.getArgumentList();
        final JSArgumentList argumentList2 = newExp2.getArgumentList();

        return (argumentList1 != null &&
                argumentList2 != null &&
                expressionListsAreEquivalent(argumentList1.getArguments(), argumentList2.getArguments()));
    }

    private static boolean arrayLiteralExpressionsAreEquivalent(JSArrayLiteralExpression arrInitExp1,
                                                                JSArrayLiteralExpression arrInitExp2) {
        return expressionListsAreEquivalent(arrInitExp1.getExpressions(), arrInitExp2.getExpressions());
    }

    private static boolean prefixExpressionsAreEquivalent(JSPrefixExpression prefixExp1,
                                                          JSPrefixExpression prefixExp2) {
        final IElementType operator1 = prefixExp1.getOperationSign();
        final IElementType operator2 = prefixExp2.getOperationSign();

        return (operator1 != null && operator2 != null &&
                operator1.equals(operator2) &&
                expressionsAreEquivalent(prefixExp1.getExpression(), prefixExp2.getExpression()));
    }

    private static boolean postfixExpressionsAreEquivalent(JSPostfixExpression postfixExp1,
                                                           JSPostfixExpression postfixExp2) {
        final IElementType operator1 = postfixExp1.getOperationSign();
        final IElementType operator2 = postfixExp2.getOperationSign();

        return (operator1.equals(operator2) &&
                expressionsAreEquivalent(postfixExp1.getExpression(), postfixExp2.getExpression()));
    }

    private static boolean binaryExpressionsAreEquivalent(JSBinaryExpression binaryExp1,
                                                          JSBinaryExpression binaryExp2) {
        final IElementType operator1 = binaryExp1.getOperationSign();
        final IElementType operator2 = binaryExp2.getOperationSign();

        return (operator1.equals(operator2)                                                  &&
                expressionsAreEquivalent(binaryExp1.getLOperand(), binaryExp2.getLOperand()) &&
                expressionsAreEquivalent(binaryExp1.getROperand(), binaryExp2.getROperand()));
    }

    private static boolean assignmentExpressionsAreEquivalent(JSAssignmentExpression assignExp1,
                                                              JSAssignmentExpression assignExp2) {
        final IElementType operator1 = assignExp1.getOperationSign();
        final IElementType operator2 = assignExp2.getOperationSign();

        return (operator1.equals(operator2)                                                  &&
                expressionsAreEquivalent(assignExp1.getLOperand(), assignExp2.getLOperand()) &&
                expressionsAreEquivalent(assignExp1.getROperand(), assignExp2.getROperand()));
    }

    private static boolean conditionalExpressionsAreEquivalent(JSConditionalExpression condExp1,
                                                               JSConditionalExpression condExp2) {
        return (expressionsAreEquivalent(condExp1.getCondition(), condExp2.getCondition()) &&
                expressionsAreEquivalent(condExp1.getThen(),      condExp2.getThen())      &&
                expressionsAreEquivalent(condExp1.getElse(),      condExp2.getElse()));
    }

    private static boolean expressionListsAreEquivalent(JSExpression[] expressions1,
                                                        JSExpression[] expressions2) {
        if (expressions1 == null && expressions2 == null) {
            return true;
        }

        if (expressions1 == null || expressions2 == null) {
            return false;
        }
        if (expressions1.length != expressions2.length) {
            return false;
        }

        for (int index = 0; index < expressions1.length; index++) {
            if (!expressionsAreEquivalent(expressions1[index], expressions2[index])) {
                return false;
            }
        }
        return true;
    }

    private static boolean areEqual(Object o1, Object o2) {
        return ((o1 == null) ? (o2 == null) : o1.equals(o2));
    }

    private static int getExpressionType(JSExpression exp) {
        if (exp instanceof JSThisExpression) {
            return THIS_EXPRESSION;
        }
        if (exp instanceof JSLiteralExpression) {
            return LITERAL_EXPRESSION;
        }
        if (exp instanceof JSReferenceExpression) {
            return REFERENCE_EXPRESSION;
        }
        if (exp instanceof JSNewExpression) {
            return NEW_EXPRESSION;
        }
        if (exp instanceof JSCallExpression) {
            return CALL_EXPRESSION;
        }
        if (exp instanceof JSArrayLiteralExpression) {
            return ARRAY_LITERAL_EXPRESSION;
        }
        if (exp instanceof JSPrefixExpression) {
            return PREFIX_EXPRESSION;
        }
        if (exp instanceof JSPostfixExpression) {
            return POSTFIX_EXPRESSION;
        }
        if (exp instanceof JSAssignmentExpression) {
            return ASSIGNMENT_EXPRESSION;
        }
        if (exp instanceof JSBinaryExpression) {
            return BINARY_EXPRESSION;
        }
        if (exp instanceof JSConditionalExpression) {
            return CONDITIONAL_EXPRESSION;
        }
        if (exp instanceof JSIndexedPropertyAccessExpression) {
            return INDEX_PROPERTY_ACCESS_EXPRESSION;
        }
        if (exp instanceof JSFunctionExpression) {
            return FUNCTION_EXPRESSION;
        }
        if (exp instanceof JSObjectLiteralExpression) {
            return OBJECT_LITERAL_EXPRESSION;
        }
        if (exp instanceof JSDefinitionExpression) {
            return DEFINITION_EXPRESSION;
        }
        return -1;
    }

    private static int getStatementType(JSStatement statement) {
        if (statement instanceof JSBlockStatement) {
            return BLOCK_STATEMENT;
        }
        if (statement instanceof JSBreakStatement) {
            return BREAK_STATEMENT;
        }
        if (statement instanceof JSContinueStatement) {
            return CONTINUE_STATEMENT;
        }
        if (statement instanceof JSVarStatement) {
            return VAR_STATEMENT;
        }
        if (statement instanceof JSDoWhileStatement) {
            return DO_WHILE_STATEMENT;
        }
        if (statement instanceof JSEmptyStatement) {
            return EMPTY_STATEMENT;
        }
        if (statement instanceof JSExpressionStatement) {
            return EXPRESSION_STATEMENT;
        }
        if (statement instanceof JSForStatement) {
            return FOR_STATEMENT;
        }
        if (statement instanceof JSForInStatement) {
            return FOR_IN_STATEMENT;
        }
        if (statement instanceof JSIfStatement) {
            return IF_STATEMENT;
        }
        if (statement instanceof JSLabeledStatement) {
            return LABELED_STATEMENT;
        }
        if (statement instanceof JSReturnStatement) {
            return RETURN_STATEMENT;
        }
        if (statement instanceof JSSwitchStatement) {
            return SWITCH_STATEMENT;
        }
        if (statement instanceof JSThrowStatement) {
            return THROW_STATEMENT;
        }
        if (statement instanceof JSTryStatement) {
            return TRY_STATEMENT;
        }
        if (statement instanceof JSWhileStatement) {
            return WHILE_STATEMENT;
        }
        if (statement instanceof JSWithStatement) {
            return WITH_STATEMENT;
        }
        return -1;
    }
}
