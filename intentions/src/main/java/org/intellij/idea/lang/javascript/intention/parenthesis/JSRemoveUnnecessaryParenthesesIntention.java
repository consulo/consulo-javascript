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
package org.intellij.idea.lang.javascript.intention.parenthesis;

import javax.annotation.Nonnull;

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

import com.intellij.lang.javascript.psi.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;

public class JSRemoveUnnecessaryParenthesesIntention extends JSIntention {
    @Override
	@Nonnull
    public JSElementPredicate getElementPredicate() {
        return new UnnecessaryParenthesesPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        JSExpression exp = (JSExpression) element;

        while (exp.getParent() instanceof JSExpression) {
            exp = (JSExpression) exp.getParent();
            assert exp != null;
        }

        final String newExpression = ParenthesesUtils.removeParentheses(exp);

        JSElementFactory.replaceExpression(exp, newExpression);
    }

    private static class UnnecessaryParenthesesPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSParenthesizedExpression)) {
                return false;
            }
            if (ErrorUtil.containsError(element)) {
                return false;
            }

            final JSParenthesizedExpression expression = (JSParenthesizedExpression) element;
            final JSElement                 parent     = (JSElement) expression.getParent();

            if (!(parent instanceof JSExpression)) {
                return true;
            }

            final JSExpression body = expression.getInnerExpression();

            if (body instanceof JSParenthesizedExpression) {
                return true;
            }

            final int parentPrecendence = ParenthesesUtils.getPrecendence((JSExpression) parent);
            final int childPrecendence  = ParenthesesUtils.getPrecendence(body);

            if (parentPrecendence > childPrecendence) {
                if (body instanceof JSFunctionExpression) return false;
                return true;
            } else if (parentPrecendence == childPrecendence) {
                if (parent instanceof JSBinaryExpression &&
                    body   instanceof JSBinaryExpression) {
                    final IElementType       parentOperator   = ((JSBinaryExpression) parent).getOperationSign();
                    final IElementType       childOperator    = ((JSBinaryExpression) body)  .getOperationSign();
                    final JSBinaryExpression binaryExpression = (JSBinaryExpression) parent;
                    final JSExpression       lhs              = binaryExpression.getLOperand();

                    return (lhs.equals(expression) && parentOperator.equals(childOperator));
                } else {
                    return false;
                }
            } else {
                return false;
            }
        }
    }
}
