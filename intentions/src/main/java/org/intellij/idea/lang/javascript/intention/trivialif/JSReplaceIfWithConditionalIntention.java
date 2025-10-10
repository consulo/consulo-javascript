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
package org.intellij.idea.lang.javascript.intention.trivialif;

import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.psi.util.PsiTreeUtil;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.*;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSReplaceIfWithConditionalIntention",
    categories = {"JavaScript", "Conditional"},
    fileExtensions = "js"
)
public class JSReplaceIfWithConditionalIntention extends JSIntention {
    @Override
    @Nonnull
    public LocalizeValue getText() {
        return JSIntentionLocalize.trivialifReplaceIfWithConditional();
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new ReplaceIfWithConditionalPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final JSIfStatement ifStatement = (JSIfStatement)(element.getParent() instanceof JSIfStatement ? element.getParent() : element);

        assert (ifStatement != null);

        if (ReplaceIfWithConditionalPredicate.isReplaceableAssignment(ifStatement)) {
            final StringBuilder newStatement = new StringBuilder();

            getAssignmentReplacement(newStatement, ifStatement);
            newStatement.append(';');
            JSElementFactory.replaceStatement(ifStatement, newStatement.toString());
        }
        else if (ReplaceIfWithConditionalPredicate.isReplaceableReturn(ifStatement)) {
            final StringBuilder newStatement = new StringBuilder("return ");

            getReturnReplacement(newStatement, ifStatement);
            newStatement.append(';');
            JSElementFactory.replaceStatement(ifStatement, newStatement.toString());
        }
        else if (ReplaceIfWithConditionalPredicate.isReplaceableImplicitReturn(ifStatement)) {
            final JSExpression condition = ifStatement.getCondition();
            final JSReturnStatement thenBranch = (JSReturnStatement)ConditionalUtils.stripBraces(ifStatement.getThen());
            final JSReturnStatement elseBranch = PsiTreeUtil.getNextSiblingOfType(ifStatement, JSReturnStatement.class);
            final String newStatement = getImplicitReturnReplacement(condition, thenBranch, elseBranch);

            JSElementFactory.replaceStatement(ifStatement, newStatement);
            if (elseBranch != null) {
                JSElementFactory.removeElement(elseBranch);
            }
        }
    }

    @RequiredReadAction
    private static void getAssignmentReplacement(StringBuilder buffer, JSIfStatement ifStatement) {
        final JSExpression condition = ifStatement.getCondition();
        final JSExpressionStatement thenBranch = (JSExpressionStatement)ConditionalUtils.stripBraces(ifStatement.getThen());
        final JSAssignmentExpression thenAssign = (JSAssignmentExpression)thenBranch.getExpression();
        final JSExpression thenRhs = thenAssign.getROperand();
        final String operator = BinaryOperatorUtils.getOperatorText(thenAssign.getOperationSign());
        final JSStatement elseBranch = ifStatement.getElse();

        assert (thenRhs != null);

        buffer.append(thenAssign.getLOperand().getText())
            .append(operator)
            .append(ParenthesesUtils.getParenthesized(condition, ParenthesesUtils.CONDITIONAL_PRECENDENCE))
            .append('?')
            .append(ParenthesesUtils.getParenthesized(thenRhs, ParenthesesUtils.CONDITIONAL_PRECENDENCE))
            .append(':');

        if (elseBranch instanceof JSIfStatement) {
            getAssignmentReplacement(buffer, (JSIfStatement)elseBranch);
            return;
        }

        final JSExpressionStatement strippedElseBranch = (JSExpressionStatement)ConditionalUtils.stripBraces(elseBranch);
        final JSAssignmentExpression elseAssign = (JSAssignmentExpression)strippedElseBranch.getExpression();
        final JSExpression elseRhs = elseAssign.getROperand();

        assert (elseRhs != null);

        buffer.append(ParenthesesUtils.getParenthesized(elseRhs, ParenthesesUtils.CONDITIONAL_PRECENDENCE));
    }

    private static void getReturnReplacement(StringBuilder buffer, JSIfStatement ifStatement) {
        final JSStatement thenBranch = ifStatement.getThen();
        final JSReturnStatement thenReturn = (JSReturnStatement)ConditionalUtils.stripBraces(thenBranch);
        final JSStatement elseBranch = ifStatement.getElse();

        buffer.append(ParenthesesUtils.getParenthesized(ifStatement.getCondition(), ParenthesesUtils.CONDITIONAL_PRECENDENCE))
            .append('?')
            .append(ParenthesesUtils.getParenthesized(thenReturn.getExpression(), ParenthesesUtils.CONDITIONAL_PRECENDENCE))
            .append(':');

        if (elseBranch instanceof JSIfStatement elseIfStatement) {
            getReturnReplacement(buffer, elseIfStatement);
            return;
        }

        final JSReturnStatement elseReturn = (JSReturnStatement)ConditionalUtils.stripBraces(elseBranch);

        buffer.append(ParenthesesUtils.getParenthesized(elseReturn.getExpression(), ParenthesesUtils.CONDITIONAL_PRECENDENCE));
    }

    private static String getImplicitReturnReplacement(JSExpression condition, JSReturnStatement thenBranch, JSReturnStatement elseBranch) {
        assert (thenBranch != null);
        assert (elseBranch != null);

        final JSExpression thenReturnValue = thenBranch.getExpression();
        final JSExpression elseReturnValue = elseBranch.getExpression();

        final String thenValue = ParenthesesUtils.getParenthesized(thenReturnValue, ParenthesesUtils.CONDITIONAL_PRECENDENCE);
        final String elseValue = ParenthesesUtils.getParenthesized(elseReturnValue, ParenthesesUtils.CONDITIONAL_PRECENDENCE);
        final String conditionText = ParenthesesUtils.getParenthesized(condition, ParenthesesUtils.CONDITIONAL_PRECENDENCE);

        return "return " + conditionText + '?' + thenValue + ':' + elseValue + ';';
    }

    private static class ReplaceIfWithConditionalPredicate implements JSElementPredicate {
        @Override
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSElement)) {
                return false;
            }

            PsiElement parent = element.getParent();

            if (!(parent instanceof JSIfStatement)) {
                if (element instanceof JSIfStatement) {
                    parent = element;
                }
                else {
                    return false;
                }
            }

            if (ErrorUtil.containsError(parent)) {
                return false;
            }

            final JSIfStatement ifStatement = (JSIfStatement)parent;
            final JSExpression condition = ifStatement.getCondition();

            if (condition == null || !condition.isValid()) {
                return false;
            }
            return isReplaceableAssignment(ifStatement)
                || isReplaceableReturn(ifStatement)
                || isReplaceableImplicitReturn(ifStatement);
        }

        public static boolean isReplaceableImplicitReturn(JSIfStatement ifStatement) {
            final PsiElement nextStatement = JSElementFactory.getNonWhiteSpaceSibling(ifStatement, true);

            if (!(nextStatement instanceof JSReturnStatement nextReturnStatement)) {
                return false;
            }

            final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());

            return thenBranch instanceof JSReturnStatement thenReturnStatement
                && thenReturnStatement.getExpression() != null
                && nextReturnStatement.getExpression() != null;
        }

        public static boolean isReplaceableReturn(JSIfStatement ifStatement) {
            final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
            final JSStatement elseBranch = ConditionalUtils.stripBraces(ifStatement.getElse());

            if (!(thenBranch instanceof JSReturnStatement thenReturnStatement)) {
                return false;
            }

            if (elseBranch instanceof JSIfStatement elseIfStatement) {
                return isReplaceableReturn(elseIfStatement);
            }

            return elseBranch instanceof JSReturnStatement elseReturnStatement
                && thenReturnStatement.getExpression() != null
                && elseReturnStatement.getExpression() != null;
        }

        @RequiredReadAction
        public static boolean isReplaceableAssignment(JSIfStatement ifStatement) {
            final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
            if (thenBranch == null) {
                return false;
            }

            if (!ConditionalUtils.isAssignment(thenBranch)) {
                return false;
            }
            final JSStatement elseBranch = ConditionalUtils.stripBraces(ifStatement.getElse());

            if (elseBranch == null) {
                return false;
            }

            if (elseBranch instanceof JSIfStatement elseIfStatement) {
                return isReplaceableAssignment(elseIfStatement);
            }

            if (!ConditionalUtils.isAssignment(elseBranch)) {
                return false;
            }

            final JSAssignmentExpression thenExpression = (JSAssignmentExpression)((JSExpressionStatement)thenBranch).getExpression();
            final JSAssignmentExpression elseExpression = (JSAssignmentExpression)((JSExpressionStatement)elseBranch).getExpression();
            final IElementType thenSign = thenExpression.getOperationSign();
            final IElementType elseSign = elseExpression.getOperationSign();

            if (!thenSign.equals(elseSign)) {
                return false;
            }

            final JSExpression thenLhs = thenExpression.getLOperand();

            if (thenExpression.getROperand() == null
                || elseExpression.getROperand() == null
                || elseExpression.getLOperand() == null) {
                return false;
            }
            final JSExpression thenRhs = thenExpression.getROperand();
            assert thenRhs != null;

            final JSExpression elseRhs = elseExpression.getROperand();
            if (elseRhs == null) {
                return false;
            }

            final JSExpression elseLhs = elseExpression.getLOperand();
            return EquivalenceChecker.expressionsAreEquivalent(thenLhs, elseLhs);
        }
    }
}
