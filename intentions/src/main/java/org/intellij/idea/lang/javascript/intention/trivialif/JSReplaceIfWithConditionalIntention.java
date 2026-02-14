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
        JSIfStatement ifStatement = (JSIfStatement)(element.getParent() instanceof JSIfStatement ? element.getParent() : element);

        assert (ifStatement != null);

        if (ReplaceIfWithConditionalPredicate.isReplaceableAssignment(ifStatement)) {
            StringBuilder newStatement = new StringBuilder();

            getAssignmentReplacement(newStatement, ifStatement);
            newStatement.append(';');
            JSElementFactory.replaceStatement(ifStatement, newStatement.toString());
        }
        else if (ReplaceIfWithConditionalPredicate.isReplaceableReturn(ifStatement)) {
            StringBuilder newStatement = new StringBuilder("return ");

            getReturnReplacement(newStatement, ifStatement);
            newStatement.append(';');
            JSElementFactory.replaceStatement(ifStatement, newStatement.toString());
        }
        else if (ReplaceIfWithConditionalPredicate.isReplaceableImplicitReturn(ifStatement)) {
            JSExpression condition = ifStatement.getCondition();
            JSReturnStatement thenBranch = (JSReturnStatement)ConditionalUtils.stripBraces(ifStatement.getThen());
            JSReturnStatement elseBranch = PsiTreeUtil.getNextSiblingOfType(ifStatement, JSReturnStatement.class);
            String newStatement = getImplicitReturnReplacement(condition, thenBranch, elseBranch);

            JSElementFactory.replaceStatement(ifStatement, newStatement);
            if (elseBranch != null) {
                JSElementFactory.removeElement(elseBranch);
            }
        }
    }

    @RequiredReadAction
    private static void getAssignmentReplacement(StringBuilder buffer, JSIfStatement ifStatement) {
        JSExpression condition = ifStatement.getCondition();
        JSExpressionStatement thenBranch = (JSExpressionStatement)ConditionalUtils.stripBraces(ifStatement.getThen());
        JSAssignmentExpression thenAssign = (JSAssignmentExpression)thenBranch.getExpression();
        JSExpression thenRhs = thenAssign.getROperand();
        String operator = BinaryOperatorUtils.getOperatorText(thenAssign.getOperationSign());
        JSStatement elseBranch = ifStatement.getElse();

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

        JSExpressionStatement strippedElseBranch = (JSExpressionStatement)ConditionalUtils.stripBraces(elseBranch);
        JSAssignmentExpression elseAssign = (JSAssignmentExpression)strippedElseBranch.getExpression();
        JSExpression elseRhs = elseAssign.getROperand();

        assert (elseRhs != null);

        buffer.append(ParenthesesUtils.getParenthesized(elseRhs, ParenthesesUtils.CONDITIONAL_PRECENDENCE));
    }

    private static void getReturnReplacement(StringBuilder buffer, JSIfStatement ifStatement) {
        JSStatement thenBranch = ifStatement.getThen();
        JSReturnStatement thenReturn = (JSReturnStatement)ConditionalUtils.stripBraces(thenBranch);
        JSStatement elseBranch = ifStatement.getElse();

        buffer.append(ParenthesesUtils.getParenthesized(ifStatement.getCondition(), ParenthesesUtils.CONDITIONAL_PRECENDENCE))
            .append('?')
            .append(ParenthesesUtils.getParenthesized(thenReturn.getExpression(), ParenthesesUtils.CONDITIONAL_PRECENDENCE))
            .append(':');

        if (elseBranch instanceof JSIfStatement elseIfStatement) {
            getReturnReplacement(buffer, elseIfStatement);
            return;
        }

        JSReturnStatement elseReturn = (JSReturnStatement)ConditionalUtils.stripBraces(elseBranch);

        buffer.append(ParenthesesUtils.getParenthesized(elseReturn.getExpression(), ParenthesesUtils.CONDITIONAL_PRECENDENCE));
    }

    private static String getImplicitReturnReplacement(JSExpression condition, JSReturnStatement thenBranch, JSReturnStatement elseBranch) {
        assert (thenBranch != null);
        assert (elseBranch != null);

        JSExpression thenReturnValue = thenBranch.getExpression();
        JSExpression elseReturnValue = elseBranch.getExpression();

        String thenValue = ParenthesesUtils.getParenthesized(thenReturnValue, ParenthesesUtils.CONDITIONAL_PRECENDENCE);
        String elseValue = ParenthesesUtils.getParenthesized(elseReturnValue, ParenthesesUtils.CONDITIONAL_PRECENDENCE);
        String conditionText = ParenthesesUtils.getParenthesized(condition, ParenthesesUtils.CONDITIONAL_PRECENDENCE);

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

            JSIfStatement ifStatement = (JSIfStatement)parent;
            JSExpression condition = ifStatement.getCondition();

            if (condition == null || !condition.isValid()) {
                return false;
            }
            return isReplaceableAssignment(ifStatement)
                || isReplaceableReturn(ifStatement)
                || isReplaceableImplicitReturn(ifStatement);
        }

        public static boolean isReplaceableImplicitReturn(JSIfStatement ifStatement) {
            PsiElement nextStatement = JSElementFactory.getNonWhiteSpaceSibling(ifStatement, true);

            if (!(nextStatement instanceof JSReturnStatement nextReturnStatement)) {
                return false;
            }

            JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());

            return thenBranch instanceof JSReturnStatement thenReturnStatement
                && thenReturnStatement.getExpression() != null
                && nextReturnStatement.getExpression() != null;
        }

        public static boolean isReplaceableReturn(JSIfStatement ifStatement) {
            JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
            JSStatement elseBranch = ConditionalUtils.stripBraces(ifStatement.getElse());

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
            JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
            if (thenBranch == null) {
                return false;
            }

            if (!ConditionalUtils.isAssignment(thenBranch)) {
                return false;
            }
            JSStatement elseBranch = ConditionalUtils.stripBraces(ifStatement.getElse());

            if (elseBranch == null) {
                return false;
            }

            if (elseBranch instanceof JSIfStatement elseIfStatement) {
                return isReplaceableAssignment(elseIfStatement);
            }

            if (!ConditionalUtils.isAssignment(elseBranch)) {
                return false;
            }

            JSAssignmentExpression thenExpression = (JSAssignmentExpression)((JSExpressionStatement)thenBranch).getExpression();
            JSAssignmentExpression elseExpression = (JSAssignmentExpression)((JSExpressionStatement)elseBranch).getExpression();
            IElementType thenSign = thenExpression.getOperationSign();
            IElementType elseSign = elseExpression.getOperationSign();

            if (!thenSign.equals(elseSign)) {
                return false;
            }

            JSExpression thenLhs = thenExpression.getLOperand();

            if (thenExpression.getROperand() == null
                || elseExpression.getROperand() == null
                || elseExpression.getLOperand() == null) {
                return false;
            }
            JSExpression thenRhs = thenExpression.getROperand();
            assert thenRhs != null;

            JSExpression elseRhs = elseExpression.getROperand();
            if (elseRhs == null) {
                return false;
            }

            JSExpression elseLhs = elseExpression.getLOperand();
            return EquivalenceChecker.expressionsAreEquivalent(thenLhs, elseLhs);
        }
    }
}
