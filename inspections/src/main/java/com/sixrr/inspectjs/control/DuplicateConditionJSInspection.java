package com.sixrr.inspectjs.control;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import com.sixrr.inspectjs.BaseInspectionVisitor;
import com.sixrr.inspectjs.JSGroupNames;
import com.sixrr.inspectjs.JavaScriptInspection;
import com.sixrr.inspectjs.localize.InspectionJSLocalize;
import com.sixrr.inspectjs.utils.EquivalenceChecker;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.language.ast.IElementType;
import consulo.language.psi.PsiElement;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

@ExtensionImpl
public class DuplicateConditionJSInspection extends JavaScriptInspection {
    @Nonnull
    @Override
    public LocalizeValue getDisplayName() {
        return InspectionJSLocalize.duplicateConditionInIfStatementDisplayName();
    }

    @Nonnull
    @Override
    public LocalizeValue getGroupDisplayName() {
        return JSGroupNames.CONTROL_FLOW_GROUP_NAME;
    }

    @Override
    @RequiredReadAction
    public String buildErrorString(Object state, Object... args) {
        return InspectionJSLocalize.duplicateConditionErrorString().get();
    }

    @Override
    public BaseInspectionVisitor buildVisitor() {
        return new DuplicateConditionVisitor();
    }

    private static class DuplicateConditionVisitor extends BaseInspectionVisitor {
        @Override
        public void visitJSIfStatement(@Nonnull JSIfStatement statement) {
            super.visitJSIfStatement(statement);
            final PsiElement parent = statement.getParent();
            if (parent instanceof JSIfStatement) {
                final JSIfStatement parentStatement = (JSIfStatement) parent;
                final JSStatement elseBranch = parentStatement.getElse();
                if (statement.equals(elseBranch)) {
                    return;
                }
            }
            final Set<JSExpression> conditions = new HashSet<JSExpression>();
            collectConditionsForIfStatement(statement, conditions);
            final int numConditions = conditions.size();
            if (numConditions < 2) {
                return;
            }
            final JSExpression[] conditionArray = conditions.toArray(new JSExpression[numConditions]);
            final boolean[] matched = new boolean[conditionArray.length];
            Arrays.fill(matched, false);
            for (int i = 0; i < conditionArray.length; i++) {
                if (matched[i]) {
                    continue;
                }
                final JSExpression condition = conditionArray[i];
                for (int j = i + 1; j < conditionArray.length; j++) {
                    if (matched[j]) {
                        continue;
                    }
                    final JSExpression testCondition = conditionArray[j];
                    final boolean areEquivalent =
                            EquivalenceChecker.expressionsAreEquivalent(condition,
                                    testCondition);
                    if (areEquivalent) {
                        registerError(testCondition);
                        if (!matched[i]) {
                            registerError(condition);
                        }
                        matched[i] = true;
                        matched[j] = true;
                    }
                }
            }
        }

        private void collectConditionsForIfStatement(JSIfStatement statement, Set<JSExpression> conditions) {
            final JSExpression condition = statement.getCondition();
            collectConditionsForExpression(condition, conditions);
            final JSStatement branch = statement.getElse();
            if (branch instanceof JSIfStatement) {
                collectConditionsForIfStatement((JSIfStatement) branch, conditions);
            }
        }

        private void collectConditionsForExpression(JSExpression condition, Set<JSExpression> conditions) {
            if (condition == null) {
                return;
            }
            if (condition instanceof JSParenthesizedExpression parenthesizedExpression) {
                final JSExpression contents = parenthesizedExpression.getInnerExpression();
                collectConditionsForExpression(contents, conditions);
                return;
            }
            if (condition instanceof JSBinaryExpression binaryExpression) {
                final IElementType sign = binaryExpression.getOperationSign();
                if (JSTokenTypes.OROR.equals(sign)) {
                    final JSExpression lhs = binaryExpression.getLOperand();
                    collectConditionsForExpression(lhs, conditions);
                    final JSExpression rhs = binaryExpression.getROperand();
                    collectConditionsForExpression(rhs, conditions);
                    return;
                }
            }
            conditions.add(condition);
        }
    }
}
