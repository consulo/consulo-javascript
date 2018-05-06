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
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;
import org.jetbrains.annotations.NonNls;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSIfStatement;
import com.intellij.lang.javascript.psi.JSStatement;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;

public class JSSplitIfOrIntention extends JSIntention {
    @NonNls private static final String IF_STATEMENT_PREFIX      = "if (";
    @NonNls private static final String ELSE_IF_STATEMENT_PREFIX = "else if (";
    @NonNls private static final String ELSE_KEYWORD             = "else ";

    @Override
	@Nonnull
    public JSElementPredicate getElementPredicate() {
        return new SplitIfOrPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final PsiElement jsElement = (element.getParent() instanceof JSIfStatement ? element.getParent() : element);

        assert (jsElement != null);
        assert (jsElement instanceof JSIfStatement);

        final JSIfStatement ifStatement = (JSIfStatement) jsElement;

        assert (ifStatement.getCondition() instanceof JSBinaryExpression);

        final JSBinaryExpression condition  = (JSBinaryExpression) ifStatement.getCondition();
        final String             lhsText    = ParenthesesUtils.removeParentheses(condition.getLOperand());
        final String             rhsText    = ParenthesesUtils.removeParentheses(condition.getROperand());
        final JSStatement        thenBranch = ifStatement.getThen();
        final JSStatement        elseBranch = ifStatement.getElse();
        final String             thenText   = thenBranch.getText();

        assert (condition.getOperationSign().equals(JSTokenTypes.OROR));

        final StringBuilder statement = new StringBuilder(ifStatement.getTextLength());

        statement.append(IF_STATEMENT_PREFIX)
                 .append(lhsText)
                 .append(')')
                 .append(thenText)
                 .append(ELSE_IF_STATEMENT_PREFIX)
                 .append(rhsText)
                 .append(')')
                 .append(thenText);
        if (elseBranch != null) {
            statement.append(ELSE_KEYWORD)
                     .append(elseBranch.getText());
        }

        JSElementFactory.replaceStatement(ifStatement, statement.toString());
    }

    private static class SplitIfOrPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            PsiElement parent = element.getParent();

            if (!(parent instanceof JSIfStatement)) {
                if (element instanceof JSIfStatement) {
                    parent = element;
                } else {
                    return false;
                }
            }

            final JSIfStatement ifStatement = (JSIfStatement) parent;
            final JSExpression  condition   = ifStatement.getCondition();

            if (condition == null || ErrorUtil.containsError(condition)) {
                return false;
            }

            return (condition instanceof JSBinaryExpression &&
                    ((JSBinaryExpression) condition).getOperationSign().equals(JSTokenTypes.OROR));
        }
    }
}
