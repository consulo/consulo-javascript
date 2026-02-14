package com.sixrr.inspectjs.utils;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;
import consulo.javascript.language.psi.JavaScriptType;
import com.intellij.lang.javascript.psi.*;
import consulo.language.psi.PsiElement;
import consulo.language.ast.IElementType;

@SuppressWarnings({"OverlyComplexMethod",
    "MethodWithMultipleLoops",
    "OverlyComplexMethod",
    "OverlyLongMethod",
    "SwitchStatementWithTooManyBranches",
    "SwitchStatement",
    "OverlyComplexClass",
    "ClassWithTooManyMethods"})
public class EquivalenceChecker {
    private EquivalenceChecker() {
        super();
    }

    private static final int THIS_EXPRESSION = 0;
    private static final int LITERAL_EXPRESSION = 1;
    private static final int CLASS_OBJECT_EXPRESSION = 2;
    private static final int REFERENCE_EXPRESSION = 3;
    private static final int SUPER_EXPRESSION = 4;
    private static final int CALL_EXPRESSION = 5;
    private static final int NEW_EXPRESSION = 6;
    private static final int ARRAY_LITERAL_EXPRESSION = 7;
    private static final int PREFIX_EXPRESSION = 10;
    private static final int POSTFIX_EXPRESSION = 11;
    private static final int BINARY_EXPRESSION = 12;
    private static final int CONDITIONAL_EXPRESSION = 13;
    private static final int ASSIGNMENT_EXPRESSION = 14;
    private static final int INDEXED_PROPERTY_ACCESS_EXPRESSION = 15;
    private static final int OBJECT_LITERAL_EXPRESSION = 16;
    private static final int FUNCTION_EXPRESSION = 17;
    private static final int DEFINITION_EXPRESSION = 18;

    private static final int BLOCK_STATEMENT = 1;
    private static final int BREAK_STATEMENT = 2;
    private static final int CONTINUE_STATEMENT = 3;
    private static final int VAR_STATEMENT = 4;
    private static final int DO_WHILE_STATEMENT = 5;
    private static final int EMPTY_STATEMENT = 6;
    private static final int EXPRESSION_STATEMENT = 8;
    private static final int FOR_STATEMENT = 9;
    private static final int IF_STATEMENT = 10;
    private static final int LABELED_STATEMENT = 11;
    private static final int RETURN_STATEMENT = 12;
    private static final int SWITCH_STATEMENT = 14;
    private static final int THROW_STATEMENT = 16;
    private static final int TRY_STATEMENT = 17;
    private static final int WHILE_STATEMENT = 18;
    private static final int FOR_EACH_STATEMENT = 19;
    private static final int WITH_STATEMENT = 20;

    public static boolean statementsAreEquivalent(@Nullable JSStatement exp1, @Nullable JSStatement exp2) {
        if (exp1 == null && exp2 == null) {
            return true;
        }
        if (exp1 == null || exp2 == null) {
            return false;
        }
        int type1 = getStatementType(exp1);
        int type2 = getStatementType(exp2);
        if (type1 != type2) {
            return false;
        }
        return switch (type1) {
            case BLOCK_STATEMENT -> blockStatementsAreEquivalent((JSBlockStatement)exp1, (JSBlockStatement)exp2);
            case BREAK_STATEMENT -> breakStatementsAreEquivalent((JSBreakStatement)exp1, (JSBreakStatement)exp2);
            case CONTINUE_STATEMENT -> continueStatementsAreEquivalent((JSContinueStatement)exp1, (JSContinueStatement)exp2);
            case VAR_STATEMENT -> varStatementsAreEquivalent((JSVarStatement)exp1, (JSVarStatement)exp2);
            case DO_WHILE_STATEMENT -> doWhileStatementsAreEquivalent((JSDoWhileStatement)exp1, (JSDoWhileStatement)exp2);
            case EMPTY_STATEMENT -> true;
            case EXPRESSION_STATEMENT -> expressionStatementsAreEquivalent((JSExpressionStatement)exp1, (JSExpressionStatement)exp2);
            case FOR_STATEMENT -> forStatementsAreEquivalent((JSForStatement)exp1, (JSForStatement)exp2);
            case FOR_EACH_STATEMENT -> forInStatementsAreEquivalent((JSForInStatement)exp1, (JSForInStatement)exp2);
            case IF_STATEMENT -> ifStatementsAreEquivalent((JSIfStatement)exp1, (JSIfStatement)exp2);
            case LABELED_STATEMENT -> labeledStatementsAreEquivalent((JSLabeledStatement)exp1, (JSLabeledStatement)exp2);
            case RETURN_STATEMENT -> returnStatementsAreEquivalent((JSReturnStatement)exp1, (JSReturnStatement)exp2);
            case SWITCH_STATEMENT -> switchStatementsAreEquivalent((JSSwitchStatement)exp1, (JSSwitchStatement)exp2);
            case THROW_STATEMENT -> throwStatementsAreEquivalent((JSThrowStatement)exp1, (JSThrowStatement)exp2);
            case TRY_STATEMENT -> tryStatementsAreEquivalent((JSTryStatement)exp1, (JSTryStatement)exp2);
            case WHILE_STATEMENT -> whileStatementsAreEquivalent((JSWhileStatement)exp1, (JSWhileStatement)exp2);
            case WITH_STATEMENT -> withStatementsAreEquivalent((JSWithStatement)exp1, (JSWithStatement)exp2);
            default -> false;
        };
    }

    private static boolean withStatementsAreEquivalent(JSWithStatement statement1, JSWithStatement statement2) {
        JSExpression expression1 = statement1.getExpression();
        JSExpression expression2 = statement2.getExpression();
        if (!expressionsAreEquivalent(expression1, expression2)) {
            return false;
        }
        JSStatement body1 = statement1.getStatement();
        JSStatement body2 = statement2.getStatement();
        return statementsAreEquivalent(body1, body2);
    }

    private static boolean varStatementsAreEquivalent(@Nonnull JSVarStatement statement1, @Nonnull JSVarStatement statement2) {
        JSVariable[] variables1 = statement1.getVariables();
        JSVariable[] variables2 = statement2.getVariables();
        if (variables1.length != variables2.length) {
            return false;
        }
        for (int i = 0; i < variables2.length; i++) {
            if (!variablesAreEquivalent(variables1[i], variables2[i])) {
                return false;
            }
        }
        return true;
    }

    private static boolean variablesAreEquivalent(@Nonnull JSVariable var1, @Nonnull JSVariable var2) {
        JSExpression initializer1 = var1.getInitializer();
        JSExpression initializer2 = var2.getInitializer();
        if (!expressionsAreEquivalent(initializer1, initializer2)) {
            return false;
        }
        JavaScriptType type1 = var1.getType();
        JavaScriptType type2 = var2.getType();
        if (!typesAreEquivalent(type1, type2)) {
            return false;
        }
        String name1 = var1.getName();
        String name2 = var2.getName();
        if (name1 == null) {
            return name2 == null;
        }
        return name1.equals(name2);
    }

    private static boolean tryStatementsAreEquivalent(@Nonnull JSTryStatement statement1, @Nonnull JSTryStatement statement2) {
        JSStatement tryBlock1 = statement1.getStatement();
        JSStatement tryBlock2 = statement2.getStatement();
        if (!statementsAreEquivalent(tryBlock1, tryBlock2)) {
            return false;
        }
        JSStatement finallyBlock1 = statement1.getFinallyStatement();
        JSStatement finallyBlock2 = statement2.getFinallyStatement();
        if (!statementsAreEquivalent(finallyBlock1, finallyBlock2)) {
            return false;
        }
        JSCatchBlock catchBlock1 = statement1.getCatchBlock();
        JSCatchBlock catchBlock2 = statement2.getCatchBlock();
        if (catchBlock1 == null) {
            return catchBlock2 == null;
        }
        if (catchBlock2 == null) {
            return false;
        }
        JSParameter parameter1 = catchBlock1.getParameter();
        JSParameter parameter2 = catchBlock2.getParameter();
        if (!parametersAreEquivalent(parameter1, parameter2)) {
            return false;
        }
        JSStatement catchStatement1 = catchBlock1.getStatement();
        JSStatement catchStatement2 = catchBlock2.getStatement();
        return statementsAreEquivalent(catchStatement1, catchStatement2);
    }

    private static boolean parametersAreEquivalent(@Nonnull JSParameter parameter1, @Nonnull JSParameter parameter2) {
        JavaScriptType type1 = parameter1.getType();
        JavaScriptType type2 = parameter2.getType();
        if (!typesAreEquivalent(type1, type2)) {
            return false;
        }
        String name1 = parameter1.getName();
        String name2 = parameter2.getName();
        if (name1 == null) {
            return name2 == null;
        }
        return name1.equals(name2);
    }

    private static boolean typesAreEquivalent(@Nullable JavaScriptType type1, @Nullable JavaScriptType type2) {
        if (type1 == null) {
            return type2 == null;
        }
        if (type2 == null) {
            return false;
        }
        return type1.equals(type2);
    }

    private static boolean whileStatementsAreEquivalent(@Nonnull JSWhileStatement statement1, @Nonnull JSWhileStatement statement2) {
        JSExpression condition1 = statement1.getCondition();
        JSExpression condition2 = statement2.getCondition();
        JSStatement body1 = statement1.getBody();
        JSStatement body2 = statement2.getBody();
        return expressionsAreEquivalent(condition1, condition2) && statementsAreEquivalent(body1, body2);
    }

    private static boolean forStatementsAreEquivalent(@Nonnull JSForStatement statement1, @Nonnull JSForStatement statement2) {
        JSExpression condition1 = statement1.getCondition();
        JSExpression condition2 = statement2.getCondition();
        if (!expressionsAreEquivalent(condition1, condition2)) {
            return false;
        }
        JSExpression initialization1 = statement1.getInitialization();
        JSExpression initialization2 = statement2.getInitialization();
        if (!expressionsAreEquivalent(initialization1, initialization2)) {
            return false;
        }
        JSExpression update1 = statement1.getUpdate();
        JSExpression update2 = statement2.getUpdate();
        if (!expressionsAreEquivalent(update1, update2)) {
            return false;
        }
        JSStatement body1 = statement1.getBody();
        JSStatement body2 = statement2.getBody();
        return statementsAreEquivalent(body1, body2);
    }

    private static boolean forInStatementsAreEquivalent(@Nonnull JSForInStatement statement1, @Nonnull JSForInStatement statement2) {
        JSExpression value1 = statement1.getCollectionExpression();
        JSExpression value2 = statement2.getCollectionExpression();
        if (!expressionsAreEquivalent(value1, value2)) {
            return false;
        }
        JSVarStatement parameter1 = statement1.getDeclarationStatement();
        JSVarStatement parameter2 = statement1.getDeclarationStatement();
        if (statementsAreEquivalent(parameter1, parameter2)) {
            return false;
        }
        JSStatement body1 = statement1.getBody();
        JSStatement body2 = statement2.getBody();
        return statementsAreEquivalent(body1, body2);
    }

    private static boolean switchStatementsAreEquivalent(@Nonnull JSSwitchStatement statement1, @Nonnull JSSwitchStatement statement2) {
        JSExpression switchExpression1 = statement1.getSwitchExpression();
        JSExpression swithcExpression2 = statement2.getSwitchExpression();
        if (!expressionsAreEquivalent(switchExpression1, swithcExpression2)) {
            return false;
        }
        JSCaseClause[] clauses1 = statement1.getCaseClauses();
        JSCaseClause[] clauses2 = statement2.getCaseClauses();
        if (clauses1.length != clauses2.length) {
            return false;
        }
        for (int i = 0; i < clauses1.length; i++) {
            JSCaseClause clause1 = clauses1[i];
            JSCaseClause clause2 = clauses2[i];
            if (!caseClausesAreEquivalent(clause1, clause2)) {
                return false;
            }
        }
        return true;
    }

    private static boolean caseClausesAreEquivalent(JSCaseClause clause1, JSCaseClause clause2) {
        if (clause1.isDefault() != clause2.isDefault()) {
            return false;
        }
        JSExpression exp1 = clause1.getCaseExpression();
        JSExpression exp2 = clause2.getCaseExpression();
        if (!expressionsAreEquivalent(exp1, exp2)) {
            return false;
        }
        JSStatement[] statements1 = clause1.getStatements();
        JSStatement[] statements2 = clause2.getStatements();
        if (statements1.length != statements2.length) {
            return false;
        }
        for (int i = 0; i < statements1.length; i++) {
            if (!statementsAreEquivalent(statements1[i], statements2[i])) {
                return false;
            }
        }
        return false;
    }

    private static boolean doWhileStatementsAreEquivalent(@Nonnull JSDoWhileStatement statement1, @Nonnull JSDoWhileStatement statement2) {
        JSExpression condition1 = statement1.getCondition();
        JSExpression condition2 = statement2.getCondition();
        JSStatement body1 = statement1.getBody();
        JSStatement body2 = statement2.getBody();
        return expressionsAreEquivalent(condition1, condition2) && statementsAreEquivalent(body1, body2);
    }

    private static boolean blockStatementsAreEquivalent(@Nonnull JSBlockStatement statement1, @Nonnull JSBlockStatement statement2) {
        JSStatement[] statements1 = statement1.getStatements();
        JSStatement[] statements2 = statement2.getStatements();
        if (statements1.length != statements2.length) {
            return false;
        }
        for (int i = 0; i < statements1.length; i++) {
            if (!statementsAreEquivalent(statements1[i], statements2[i])) {
                return false;
            }
        }
        return true;
    }

    private static boolean breakStatementsAreEquivalent(@Nonnull JSBreakStatement statement1, @Nonnull JSBreakStatement statement2) {
        String identifier1 = statement1.getLabel();
        String identifier2 = statement2.getLabel();
        if (identifier1 == null) {
            return identifier2 == null;
        }
        if (identifier2 == null) {
            return false;
        }
        return identifier1.equals(identifier2);
    }

    private static boolean continueStatementsAreEquivalent(
        @Nonnull JSContinueStatement statement1,
        @Nonnull JSContinueStatement statement2
    ) {
        String identifier1 = statement1.getLabel();
        String identifier2 = statement2.getLabel();
        if (identifier1 == null) {
            return identifier2 == null;
        }
        if (identifier2 == null) {
            return false;
        }
        return identifier1.equals(identifier2);
    }

    private static boolean labeledStatementsAreEquivalent(@Nonnull JSLabeledStatement statement1, @Nonnull JSLabeledStatement statement2) {
        PsiElement identifier1 = statement1.getLabelIdentifier();
        PsiElement identifier2 = statement2.getLabelIdentifier();
        if (identifier1 == null) {
            return identifier2 == null;
        }
        if (identifier2 == null) {
            return false;
        }
        String text1 = identifier1.getText();
        String text2 = identifier2.getText();
        return text1.equals(text2);
    }

    private static boolean ifStatementsAreEquivalent(@Nonnull JSIfStatement statement1, @Nonnull JSIfStatement statement2) {
        JSExpression condition1 = statement1.getCondition();
        JSExpression condition2 = statement2.getCondition();
        JSStatement thenBranch1 = statement1.getThen();
        JSStatement thenBranch2 = statement2.getThen();
        JSStatement elseBranch1 = statement1.getElse();
        JSStatement elseBranch2 = statement2.getElse();
        return expressionsAreEquivalent(condition1, condition2)
            && statementsAreEquivalent(thenBranch1, thenBranch2)
            && statementsAreEquivalent(elseBranch1, elseBranch2);
    }

    private static boolean expressionStatementsAreEquivalent(
        @Nonnull JSExpressionStatement statement1,
        @Nonnull JSExpressionStatement statement2
    ) {
        JSExpression expression1 = statement1.getExpression();
        JSExpression expression2 = statement2.getExpression();
        return expressionsAreEquivalent(expression1, expression2);
    }

    private static boolean returnStatementsAreEquivalent(@Nonnull JSReturnStatement statement1, @Nonnull JSReturnStatement statement2) {
        JSExpression returnValue1 = statement1.getExpression();
        JSExpression returnValue2 = statement2.getExpression();
        return expressionsAreEquivalent(returnValue1, returnValue2);
    }

    private static boolean throwStatementsAreEquivalent(@Nonnull JSThrowStatement statement1, @Nonnull JSThrowStatement statement2) {
        JSExpression exception1 = statement1.getExpression();
        JSExpression exception2 = statement2.getExpression();
        return expressionsAreEquivalent(exception1, exception2);
    }

    public static boolean expressionsAreEquivalent(@Nullable JSExpression exp1, @Nullable JSExpression exp2) {
        if (exp1 == null && exp2 == null) {
            return true;
        }
        if (exp1 == null || exp2 == null) {
            return false;
        }
        JSExpression expToCompare1 = exp1;
        while (expToCompare1 instanceof JSParenthesizedExpression parenExp1) {
            expToCompare1 = parenExp1.getInnerExpression();
        }
        JSExpression expToCompare2 = exp2;
        while (expToCompare2 instanceof JSParenthesizedExpression parenExp2) {
            expToCompare2 = parenExp2.getInnerExpression();
        }
        int type1 = getExpressionType(expToCompare1);
        int type2 = getExpressionType(expToCompare2);
        if (type1 != type2) {
            return false;
        }
        return switch (type1) {
            case THIS_EXPRESSION, SUPER_EXPRESSION -> true;
            case LITERAL_EXPRESSION, CLASS_OBJECT_EXPRESSION, REFERENCE_EXPRESSION ->
                expToCompare1.getText().equals(expToCompare2.getText());
            case CALL_EXPRESSION -> methodCallExpressionsAreEquivalent((JSCallExpression)expToCompare1, (JSCallExpression)expToCompare2);
            case NEW_EXPRESSION -> newExpressionsAreEquivalent((JSNewExpression)expToCompare1, (JSNewExpression)expToCompare2);
            case ARRAY_LITERAL_EXPRESSION -> arrayInitializerExpressionsAreEquivalent(
                    (JSArrayLiteralExpression)expToCompare1,
                    (JSArrayLiteralExpression)expToCompare2
                );
            case PREFIX_EXPRESSION -> prefixExpressionsAreEquivalent((JSPrefixExpression)expToCompare1, (JSPrefixExpression)expToCompare2);
            case POSTFIX_EXPRESSION ->
                postfixExpressionsAreEquivalent((JSPostfixExpression)expToCompare1, (JSPostfixExpression)expToCompare2);
            case BINARY_EXPRESSION -> binaryExpressionsAreEquivalent((JSBinaryExpression)expToCompare1, (JSBinaryExpression)expToCompare2);
            case ASSIGNMENT_EXPRESSION ->
                assignmentExpressionsAreEquivalent((JSAssignmentExpression)expToCompare1, (JSAssignmentExpression)expToCompare2);
            case CONDITIONAL_EXPRESSION -> conditionalExpressionsAreEquivalent(
                    (JSConditionalExpression)expToCompare1,
                    (JSConditionalExpression)expToCompare2
                );
            case INDEXED_PROPERTY_ACCESS_EXPRESSION -> indexedAccessExpressionsAreEquivalent(
                    (JSIndexedPropertyAccessExpression)expToCompare1,
                    (JSIndexedPropertyAccessExpression)expToCompare2
                );
            case OBJECT_LITERAL_EXPRESSION -> objectLiteralExpressionsAreEquivalent(
                    (JSObjectLiteralExpression)expToCompare1,
                    (JSObjectLiteralExpression)expToCompare2
                );
            case FUNCTION_EXPRESSION ->
                functionExpressionsAreEquivalent((JSFunctionExpression)expToCompare1, (JSFunctionExpression)expToCompare2);
            case DEFINITION_EXPRESSION ->
                definitionExpressionsAreEquivalent((JSDefinitionExpression)expToCompare1, (JSDefinitionExpression)expToCompare2);
            default -> false;
        };
    }

    private static boolean definitionExpressionsAreEquivalent(
        JSDefinitionExpression jsDefinitionExpression,
        JSDefinitionExpression jsDefinitionExpression1
    ) {
        return expressionsAreEquivalent(jsDefinitionExpression.getExpression(), jsDefinitionExpression1.getExpression());
    }

    private static boolean functionExpressionsAreEquivalent(JSFunctionExpression exp1, JSFunctionExpression exp2) {
        return false;
    }

    private static boolean objectLiteralExpressionsAreEquivalent(JSObjectLiteralExpression exp1, JSObjectLiteralExpression exp2) {
        JSProperty[] properties1 = exp1.getProperties();
        JSProperty[] properties2 = exp2.getProperties();
        if (properties1.length != properties2.length) {
            return false;
        }
        for (int i = 0; i < properties2.length; i++) {
            JSProperty property1 = properties1[i];
            JSProperty property2 = properties2[i];
            if (!property2.getName().equals(property1.getName())) {
                return false;
            }
            JSExpression value1 = property1.getValue();
            JSExpression value2 = property2.getValue();
            if (!expressionsAreEquivalent(value1, value2)) {
                return false;
            }
        }
        return true;
    }

    private static boolean indexedAccessExpressionsAreEquivalent(
        JSIndexedPropertyAccessExpression exp1,
        JSIndexedPropertyAccessExpression exp2
    ) {
        JSExpression index1 = exp1.getIndexExpression();
        JSExpression index2 = exp2.getIndexExpression();
        if (!expressionsAreEquivalent(index1, index2)) {
            return false;
        }
        JSExpression qualifier1 = exp1.getQualifier();
        JSExpression qualifier2 = exp2.getQualifier();
        return expressionsAreEquivalent(qualifier1, qualifier2);
    }

    private static boolean methodCallExpressionsAreEquivalent(
        @Nonnull JSCallExpression methodExp1,
        @Nonnull JSCallExpression methodExp2
    ) {
        JSExpression methodExpression1;
        JSExpression methodExpression2;
        try {
            methodExpression1 = methodExp1.getMethodExpression();
            methodExpression2 = methodExp2.getMethodExpression();
        }
        catch (Exception e) {
            return false; //catching an intelliJ CCE
        }
        if (!expressionsAreEquivalent(methodExpression1, methodExpression2)) {
            return false;
        }
        JSArgumentList argumentList1 = methodExp1.getArgumentList();
        if (argumentList1 == null) {
            return false;
        }
        JSExpression[] args1 = argumentList1.getArguments();
        JSArgumentList argumentList2 = methodExp2.getArgumentList();
        if (argumentList2 == null) {
            return false;
        }
        JSExpression[] args2 = argumentList2.getArguments();
        return expressionListsAreEquivalent(args1, args2);
    }

    private static boolean newExpressionsAreEquivalent(@Nonnull JSNewExpression newExp1, @Nonnull JSNewExpression newExp2) {
        JSExpression methodExpression1 = newExp1.getMethodExpression();
        JSExpression methodExpression2 = newExp2.getMethodExpression();
        if (!expressionsAreEquivalent(methodExpression1, methodExpression2)) {
            return false;
        }
        JSArgumentList argumentList1 = newExp1.getArgumentList();
        if (argumentList1 == null) {
            return false;
        }
        JSExpression[] args1 = argumentList1.getArguments();
        JSArgumentList argumentList2 = newExp1.getArgumentList();
        if (argumentList2 == null) {
            return false;
        }
        JSExpression[] args2 = argumentList2.getArguments();
        return expressionListsAreEquivalent(args1, args2);
    }

    private static boolean arrayInitializerExpressionsAreEquivalent(
        @Nonnull JSArrayLiteralExpression arrInitExp1,
        @Nonnull JSArrayLiteralExpression arrInitExp2
    ) {
        JSExpression[] initializers1 = arrInitExp1.getExpressions();
        JSExpression[] initializers2 = arrInitExp2.getExpressions();
        return expressionListsAreEquivalent(initializers1, initializers2);
    }

    private static boolean prefixExpressionsAreEquivalent(
        @Nonnull JSPrefixExpression prefixExp1,
        @Nonnull JSPrefixExpression prefixExp2
    ) {
        IElementType sign1 = prefixExp1.getOperationSign();
        IElementType sign2 = prefixExp2.getOperationSign();
        if (sign1 == null) {
            return sign1 == sign2;
        }
        if (!sign1.equals(sign2)) {
            return false;
        }
        JSExpression operand1 = prefixExp1.getExpression();
        JSExpression operand2 = prefixExp2.getExpression();
        return expressionsAreEquivalent(operand1, operand2);
    }

    private static boolean postfixExpressionsAreEquivalent(
        @Nonnull JSPostfixExpression postfixExp1,
        @Nonnull JSPostfixExpression postfixExp2
    ) {
        IElementType sign1 = postfixExp1.getOperationSign();
        IElementType sign2 = postfixExp2.getOperationSign();
        if (!sign1.equals(sign2)) {
            return false;
        }
        JSExpression operand1 = postfixExp1.getExpression();
        JSExpression operand2 = postfixExp2.getExpression();
        return expressionsAreEquivalent(operand1, operand2);
    }

    private static boolean binaryExpressionsAreEquivalent(@Nonnull JSBinaryExpression binaryExp1, @Nonnull JSBinaryExpression binaryExp2) {
        IElementType sign1 = binaryExp1.getOperationSign();
        IElementType sign2 = binaryExp2.getOperationSign();
        if (!sign1.equals(sign2)) {
            return false;
        }
        JSExpression lhs1 = binaryExp1.getLOperand();
        JSExpression lhs2 = binaryExp2.getLOperand();
        JSExpression rhs1 = binaryExp1.getROperand();
        JSExpression rhs2 = binaryExp2.getROperand();
        return expressionsAreEquivalent(lhs1, lhs2) && expressionsAreEquivalent(rhs1, rhs2);
    }

    private static boolean assignmentExpressionsAreEquivalent(
        @Nonnull JSAssignmentExpression assignExp1,
        @Nonnull JSAssignmentExpression assignExp2
    ) {
        IElementType sign1 = assignExp1.getOperationSign();
        IElementType sign2 = assignExp2.getOperationSign();
        if (!sign1.equals(sign2)) {
            return false;
        }
        JSExpression lhs1 = assignExp1.getLOperand();
        JSExpression lhs2 = assignExp2.getLOperand();
        JSExpression rhs1 = assignExp1.getROperand();
        JSExpression rhs2 = assignExp2.getROperand();
        return expressionsAreEquivalent(lhs1, lhs2)
            && expressionsAreEquivalent(rhs1, rhs2);
    }

    private static boolean conditionalExpressionsAreEquivalent(
        @Nonnull JSConditionalExpression condExp1,
        @Nonnull JSConditionalExpression condExp2
    ) {
        JSExpression condition1 = condExp1.getCondition();
        JSExpression condition2 = condExp2.getCondition();
        JSExpression thenExpression1 = condExp1.getThen();
        JSExpression thenExpression2 = condExp2.getThen();
        JSExpression elseExpression1 = condExp1.getElse();
        JSExpression elseExpression2 = condExp2.getElse();
        return expressionsAreEquivalent(condition1, condition2)
            && expressionsAreEquivalent(thenExpression1, thenExpression2)
            && expressionsAreEquivalent(elseExpression1, elseExpression2);
    }

    private static boolean expressionListsAreEquivalent(@Nullable JSExpression[] expressions1, @Nullable JSExpression[] expressions2) {
        if (expressions1 == null && expressions2 == null) {
            return true;
        }
        if (expressions1 == null || expressions2 == null) {
            return false;
        }
        if (expressions1.length != expressions2.length) {
            return false;
        }
        for (int i = 0; i < expressions1.length; i++) {
            if (!expressionsAreEquivalent(expressions1[i], expressions2[i])) {
                return false;
            }
        }
        return true;
    }

    private static int getExpressionType(@Nullable JSExpression exp) {
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
            return INDEXED_PROPERTY_ACCESS_EXPRESSION;
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

    private static int getStatementType(@Nullable JSStatement statement) {

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
            return FOR_EACH_STATEMENT;
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
