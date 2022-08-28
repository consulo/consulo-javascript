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

import javax.annotation.Nonnull;

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ControlFlowUtils;
import org.intellij.idea.lang.javascript.psiutil.EquivalenceChecker;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NonNls;

import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;

public class JSMergeIfOrIntention extends JSIntention {
    @NonNls private static final String IF_STATEMENT_PREFIX = "if (";
    @NonNls private static final String ELSE_KEYWORD        = "else ";

    @Override
	@Nonnull
    public JSElementPredicate getElementPredicate() {
        return new MergeIfOrPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        assert (element instanceof JSElement);
        JSElement jsElement = (JSElement) element;
        if (MergeIfOrPredicate.isMergableExplicitIf(jsElement)) {
            replaceMergeableExplicitIf(jsElement);
        } else {
            replaceMergeableImplicitIf(jsElement);
        }
    }

    private static void replaceMergeableExplicitIf(JSElement token) throws IncorrectOperationException {
        final JSIfStatement parentStatement = (JSIfStatement) (token.getParent() instanceof JSIfStatement ? token.getParent() : token);

        assert (parentStatement != null);

        final JSIfStatement childStatement       = (JSIfStatement) parentStatement.getElse();
        final JSExpression  childCondition       = childStatement.getCondition();
        final JSExpression  condition            = parentStatement.getCondition();
        final String        childConditionText   = ParenthesesUtils.getParenthesized(childCondition, ParenthesesUtils.OR_PRECENDENCE);
        final String        parentConditionText  = ParenthesesUtils.getParenthesized(condition,      ParenthesesUtils.OR_PRECENDENCE);
        final JSStatement   parentThenBranch     = parentStatement.getThen();
        final String        parentThenBranchText = parentThenBranch.getText();
        final StringBuilder statement            = new StringBuilder(IF_STATEMENT_PREFIX)
                                                             .append(parentConditionText)
                                                             .append(" || ")
                                                             .append(childConditionText)
                                                             .append(')')
                                                             .append(parentThenBranchText);
        final JSStatement   childElseBranch      = childStatement.getElse();

        if (childElseBranch != null) {
            final String childElseBranchText = childElseBranch.getText();

            statement.append(ELSE_KEYWORD)
                     .append(childElseBranchText);
        }

        JSElementFactory.replaceStatement(parentStatement, statement.toString());
    }

    private static void replaceMergeableImplicitIf(JSElement token) throws IncorrectOperationException {
        final JSIfStatement parentStatement = (JSIfStatement) (token.getParent() instanceof JSIfStatement ? token.getParent() : token);
        final JSIfStatement childStatement  = (JSIfStatement) JSElementFactory.getNonWhiteSpaceSibling(parentStatement, true);

        assert (childStatement  != null);
        assert (parentStatement != null);

        final JSExpression childCondition      = childStatement.getCondition();
        final JSExpression condition           = parentStatement.getCondition();
        final String       childConditionText  = ParenthesesUtils.getParenthesized(childCondition, ParenthesesUtils.OR_PRECENDENCE);
        final String       parentConditionText = ParenthesesUtils.getParenthesized(condition,      ParenthesesUtils.OR_PRECENDENCE);
        final JSStatement  parentThenBranch    = parentStatement.getThen();
        final JSStatement  childElseBranch     = childStatement.getElse();
        StringBuilder      statement           = new StringBuilder(IF_STATEMENT_PREFIX)
                                                           .append(parentConditionText)
                                                           .append(" || ")
                                                           .append(childConditionText)
                                                           .append(')')
                                                           .append(parentThenBranch.getText());

        if (childElseBranch != null) {
            statement.append(ELSE_KEYWORD)
                     .append(childElseBranch.getText());
        }

        JSElementFactory.replaceStatement(parentStatement, statement.toString());
        JSElementFactory.removeElement(childStatement);
    }

    private static class MergeIfOrPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSElement)) {
                return false;
            }

            final JSElement jsElement  = (JSElement) element;

            return (isMergableExplicitIf(jsElement) || isMergableImplicitIf(jsElement));
        }

        public static boolean isMergableExplicitIf(JSElement element) {
            PsiElement parent = element.getParent();

            if (!(parent instanceof JSIfStatement)) {
                if (element instanceof JSIfStatement) {
                    parent = element;
                } else {
                    return false;
                }
            }

            final JSIfStatement ifStatement = (JSIfStatement) parent;

            if (ErrorUtil.containsError(ifStatement)) {
                return false;
            }

            final JSStatement thenBranch = ifStatement.getThen();
            final JSStatement elseBranch = ifStatement.getElse();

            if (thenBranch == null || elseBranch == null) {
                return false;
            }
            if (!(elseBranch instanceof JSIfStatement)) {
                return false;
            }

            final JSIfStatement childIfStatement = (JSIfStatement) elseBranch;
            final JSStatement   childThenBranch  = childIfStatement.getThen();

            return EquivalenceChecker.statementsAreEquivalent(thenBranch, childThenBranch);
        }

        private static boolean isMergableImplicitIf(JSElement element) {
            PsiElement parent = element.getParent();

            if (!(parent instanceof JSIfStatement)) {
                if (element instanceof JSIfStatement) {
                    parent = element;
                } else {
                    return false;
                }
            }

            final JSIfStatement ifStatement = (JSIfStatement) parent;
            final JSStatement   thenBranch  = ifStatement.getThen();
            final JSStatement   elseBranch  = ifStatement.getElse();

            if (thenBranch == null || elseBranch != null) {
                return false;
            }

            if (ControlFlowUtils.statementMayCompleteNormally(thenBranch)) {
                return false;
            }

            final PsiElement nextStatement = JSElementFactory.getNonWhiteSpaceSibling(ifStatement, true);

            if (!(nextStatement instanceof JSIfStatement)) {
                return false;
            }

            final JSIfStatement childIfStatement = (JSIfStatement) nextStatement;
            final JSStatement   childThenBranch  = childIfStatement.getThen();

            return EquivalenceChecker.statementsAreEquivalent(thenBranch, childThenBranch);
        }
    }
}