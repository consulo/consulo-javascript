package com.sixrr.inspectjs.utils;

import com.intellij.lang.javascript.psi.*;
import org.jetbrains.annotations.NonNls;

@NonNls
public class ConditionalUtils {
    private ConditionalUtils() {
        super();
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

    public static boolean isReturn(JSStatement statement, String value) {
        if (statement == null) {
            return false;
        }
        if (!(statement instanceof JSReturnStatement)) {
            return false;
        }
        JSReturnStatement returnStatement = (JSReturnStatement)statement;
        if (returnStatement.getExpression() == null) {
            return false;
        }
        JSExpression returnValue = returnStatement.getExpression();
        String returnValueText = returnValue.getText();
        return value.equals(returnValueText);
    }

    public static boolean isAssignment(JSStatement statement, String value) {
        if (statement == null) {
            return false;
        }
        if (!(statement instanceof JSExpressionStatement)) {
            return false;
        }
        JSExpressionStatement expressionStatement = (JSExpressionStatement)statement;
        JSExpression expression = expressionStatement.getExpression();
        if (!(expression instanceof JSAssignmentExpression)) {
            return false;
        }
        JSAssignmentExpression assignment = (JSAssignmentExpression)expression;
        JSExpression rhs = assignment.getROperand();
        if (rhs == null) {
            return false;
        }
        String rhsText = rhs.getText();
        return value.equals(rhsText);
    }
}
