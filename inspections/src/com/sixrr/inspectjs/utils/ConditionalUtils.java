package com.sixrr.inspectjs.utils;

import com.intellij.lang.javascript.psi.*;
import org.jetbrains.annotations.NonNls;

@NonNls
public class ConditionalUtils {
    private ConditionalUtils() {
        super();
    }

    public static JSStatement stripBraces(JSStatement branch) {
        if (branch instanceof JSBlockStatement) {
            final JSBlockStatement block = (JSBlockStatement) branch;
            final JSStatement[] statements = block.getStatements();
            if (statements.length == 1) {
                return statements[0];
            } else {
                return block;
            }
        } else {
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
        final JSReturnStatement returnStatement =
                (JSReturnStatement) statement;
        if (returnStatement.getExpression() == null) {
            return false;
        }
        final JSExpression returnValue = returnStatement.getExpression();
        final String returnValueText = returnValue.getText();
        return value.equals(returnValueText);
    }

    public static boolean isAssignment(JSStatement statement, String value) {
        if (statement == null) {
            return false;
        }
        if (!(statement instanceof JSExpressionStatement)) {
            return false;
        }
        final JSExpressionStatement expressionStatement =
                (JSExpressionStatement) statement;
        final JSExpression expression = expressionStatement.getExpression();
        if (!(expression instanceof JSAssignmentExpression)) {
            return false;
        }
        final JSAssignmentExpression assignment =
                (JSAssignmentExpression) expression;
        final JSExpression rhs = assignment.getROperand();
        if (rhs == null) {
            return false;
        }
        final String rhsText = rhs.getText();
        return value.equals(rhsText);
    }
}
