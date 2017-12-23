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

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.ConditionalUtils;
import org.intellij.idea.lang.javascript.psiutil.EquivalenceChecker;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.NonNls;

import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSExpressionStatement;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSReturnStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.IncorrectOperationException;

public class JSReplaceIfWithConditionalIntention extends JSIntention {
    private         static final char   TERNARY_QUESTION  = '?';
    private         static final char   TERNARY_SEMICOLON = ':';
    @NonNls private static final String RETURN_KEYWORD    = "return ";

    @Override
	@NotNull
    public JSElementPredicate getElementPredicate() {
        return new ReplaceIfWithConditionalPredicate();
    }

    @Override
	public void processIntention(@NotNull PsiElement element) throws IncorrectOperationException {
        final JSIfStatement ifStatement = (JSIfStatement) (element.getParent() instanceof JSIfStatement ? element.getParent() : element);

        assert (ifStatement != null);

        if (ReplaceIfWithConditionalPredicate.isReplaceableAssignment(ifStatement)) {
            final StringBuilder newStatement = new StringBuilder();

            getAssignmentReplacement(newStatement, ifStatement);
            newStatement.append(';');
            JSElementFactory.replaceStatement(ifStatement, newStatement.toString());
        } else if (ReplaceIfWithConditionalPredicate.isReplaceableReturn(ifStatement)) {
            final StringBuilder newStatement = new StringBuilder(RETURN_KEYWORD);

            getReturnReplacement(newStatement, ifStatement);
            newStatement.append(';');
            JSElementFactory.replaceStatement(ifStatement, newStatement.toString());
        } else if (ReplaceIfWithConditionalPredicate.isReplaceableImplicitReturn(ifStatement)) {
            final JSExpression      condition    = ifStatement.getCondition();
            final JSReturnStatement thenBranch   = (JSReturnStatement) ConditionalUtils.stripBraces(ifStatement.getThen());
            final JSReturnStatement elseBranch   = PsiTreeUtil.getNextSiblingOfType(ifStatement, JSReturnStatement.class);
            final String            newStatement = getImplicitReturnReplacement(condition, thenBranch, elseBranch);

            JSElementFactory.replaceStatement(ifStatement, newStatement);
            if (elseBranch != null) {
                JSElementFactory.removeElement(elseBranch);
            }
        }
    }

    private static void getAssignmentReplacement(StringBuilder buffer, JSIfStatement ifStatement) {
        final JSExpression           condition  = ifStatement.getCondition();
        final JSExpressionStatement  thenBranch = (JSExpressionStatement) ConditionalUtils.stripBraces(ifStatement.getThen());
        final JSAssignmentExpression thenAssign = (JSAssignmentExpression) thenBranch.getExpression();
        final JSExpression           thenRhs    = thenAssign.getROperand();
        final String                 operator   = BinaryOperatorUtils.getOperatorText(thenAssign.getOperationSign());
        final JSStatement            elseBranch = ifStatement.getElse();

        assert (thenRhs != null);

        buffer.append(thenAssign.getLOperand().getText())
              .append(operator)
              .append(ParenthesesUtils.getParenthesized(condition, ParenthesesUtils.CONDITIONAL_PRECENDENCE))
              .append(TERNARY_QUESTION)
              .append(ParenthesesUtils.getParenthesized(thenRhs,   ParenthesesUtils.CONDITIONAL_PRECENDENCE))
              .append(TERNARY_SEMICOLON);

        if (elseBranch instanceof JSIfStatement) {
            getAssignmentReplacement(buffer, (JSIfStatement) elseBranch);
            return;
        }

        final JSExpressionStatement  strippedElseBranch = (JSExpressionStatement)  ConditionalUtils.stripBraces(elseBranch);
        final JSAssignmentExpression elseAssign         = (JSAssignmentExpression) strippedElseBranch.getExpression();
        final JSExpression           elseRhs            = elseAssign.getROperand();

        assert (elseRhs != null);

        buffer.append(ParenthesesUtils.getParenthesized(elseRhs, ParenthesesUtils.CONDITIONAL_PRECENDENCE));
    }

    private static void getReturnReplacement(StringBuilder buffer, JSIfStatement ifStatement) {
        final JSStatement       thenBranch = ifStatement.getThen();
        final JSReturnStatement thenReturn = (JSReturnStatement) ConditionalUtils.stripBraces(thenBranch);
        final JSStatement       elseBranch = ifStatement.getElse();

        buffer.append(ParenthesesUtils.getParenthesized(ifStatement.getCondition(),
                                                        ParenthesesUtils.CONDITIONAL_PRECENDENCE))
              .append(TERNARY_QUESTION)
              .append(ParenthesesUtils.getParenthesized(thenReturn.getExpression(),
                                                        ParenthesesUtils.CONDITIONAL_PRECENDENCE))
              .append(TERNARY_SEMICOLON);

        if (elseBranch instanceof JSIfStatement) {
            getReturnReplacement(buffer, (JSIfStatement) elseBranch);
            return;
        }

        final JSReturnStatement elseReturn = (JSReturnStatement) ConditionalUtils.stripBraces(elseBranch);

        buffer.append(ParenthesesUtils.getParenthesized(elseReturn.getExpression(),
                                                        ParenthesesUtils.CONDITIONAL_PRECENDENCE));
    }

    private static String getImplicitReturnReplacement(JSExpression condition, JSReturnStatement thenBranch, JSReturnStatement elseBranch) {
        assert (thenBranch != null);
        assert (elseBranch != null);

        final JSExpression thenReturnValue = thenBranch.getExpression();
        final JSExpression elseReturnValue = elseBranch.getExpression();

        final String thenValue     = ParenthesesUtils.getParenthesized(thenReturnValue, ParenthesesUtils.CONDITIONAL_PRECENDENCE);
        final String elseValue     = ParenthesesUtils.getParenthesized(elseReturnValue, ParenthesesUtils.CONDITIONAL_PRECENDENCE);
        final String conditionText = ParenthesesUtils.getParenthesized(condition,       ParenthesesUtils.CONDITIONAL_PRECENDENCE);

        return RETURN_KEYWORD + conditionText + TERNARY_QUESTION + thenValue + TERNARY_SEMICOLON + elseValue + ';';
    }

    private static class ReplaceIfWithConditionalPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@NotNull PsiElement element) {
            if (!(element instanceof JSElement)) {
                return false;
            }

            PsiElement parent = element.getParent();

            if (!(parent instanceof JSIfStatement)) {
                if (element instanceof JSIfStatement) {
                    parent = element;
                } else {
                    return false;
                }
            }

            if (ErrorUtil.containsError(parent)) {
                return false;
            }

            final JSIfStatement ifStatement = (JSIfStatement) parent;
            final JSExpression  condition   = ifStatement.getCondition();

            if (condition == null || !condition.isValid()) {
                return false;
            }
            if (isReplaceableAssignment(ifStatement)) {
                return true;
            }
            if (isReplaceableReturn(ifStatement)) {
                return true;
            }
            return isReplaceableImplicitReturn(ifStatement);
        }

        public static boolean isReplaceableImplicitReturn(JSIfStatement ifStatement) {
            final PsiElement nextStatement = JSElementFactory.getNonWhiteSpaceSibling(ifStatement, true);

            if (!(nextStatement instanceof JSReturnStatement)) {
                return false;
            }

            final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());

            if (!(thenBranch instanceof JSReturnStatement)) {
                return false;
            }

            return (((JSReturnStatement) thenBranch)   .getExpression() != null &&
                    ((JSReturnStatement) nextStatement).getExpression() != null);
        }

        public static boolean isReplaceableReturn(JSIfStatement ifStatement) {
            final JSStatement thenBranch = ConditionalUtils.stripBraces(ifStatement.getThen());
            final JSStatement elseBranch = ConditionalUtils.stripBraces(ifStatement.getElse());

            if (!(thenBranch instanceof JSReturnStatement)) {
                return false;
            }

            if (elseBranch instanceof JSIfStatement) {
                return isReplaceableReturn((JSIfStatement) elseBranch);
            }
            if (!(elseBranch instanceof JSReturnStatement)) {
                return false;
            }

            return (((JSReturnStatement) thenBranch).getExpression() != null &&
                    ((JSReturnStatement) elseBranch).getExpression() != null);

        }

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
            if (elseBranch instanceof JSIfStatement) {
                return isReplaceableAssignment((JSIfStatement) elseBranch);
            }

            if (!ConditionalUtils.isAssignment(elseBranch)) {
                return false;
            }

            final JSAssignmentExpression thenExpression = (JSAssignmentExpression) ((JSExpressionStatement) thenBranch).getExpression();
            final JSAssignmentExpression elseExpression = (JSAssignmentExpression) ((JSExpressionStatement) elseBranch).getExpression();
            final IElementType           thenSign       = thenExpression.getOperationSign();
            final IElementType           elseSign       = elseExpression.getOperationSign();

            if (!thenSign.equals(elseSign)) {
                return false;
            }

            final JSExpression thenLhs = thenExpression.getLOperand();

            if (thenExpression.getROperand() == null) {
                return false;
            }
            if (elseExpression.getROperand() == null ||
                elseExpression.getLOperand() == null) {
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
