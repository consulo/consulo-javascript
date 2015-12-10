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
package org.intellij.idea.lang.javascript.intention.constant;

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.ExpressionUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.impl.JSChangeUtil;
import com.intellij.lang.javascript.psi.util.JSUtils;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;

public class JSConstantSubexpressionIntention extends JSMutablyNamedIntention {
    @Override
	@NotNull
    protected JSElementPredicate getElementPredicate() {
        return new ConstantSubexpressionPredicate();
    }

    @Override
	protected String getTextForElement(PsiElement element) {
        final PsiElement         parent           = element.getParent();
        final JSBinaryExpression binaryExpression = (JSBinaryExpression) (parent instanceof JSBinaryExpression ? parent : element);
        final JSExpression       lhs              = binaryExpression.getLOperand();
        final JSExpression       leftSide;

        if (lhs instanceof JSBinaryExpression) {
            leftSide = ((JSBinaryExpression) lhs).getROperand();
        } else {
            leftSide = lhs;
        }
        final IElementType operationSign = binaryExpression.getOperationSign();
        final JSExpression rhs           = binaryExpression.getROperand();

        assert (rhs != null);
        assert (leftSide != null);

        return this.getText(leftSide.getText(), BinaryOperatorUtils.getOperatorText(operationSign), rhs.getText());
    }

    @Override
	public void processIntention(@NotNull PsiElement element) throws IncorrectOperationException {
        final PsiElement   parent        = element.getParent();
        final JSExpression expression    = (JSExpression) (parent instanceof JSBinaryExpression ? parent : element);
        String             newExpression = "";
        final Object       constantValue;

        if (expression instanceof JSBinaryExpression) {
            final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;
            final JSExpression       lhs              = binaryExpression.getLOperand();

            if (lhs instanceof JSBinaryExpression) {
                final JSBinaryExpression lhsBinaryExpression = (JSBinaryExpression) lhs;
                final JSExpression       rightSide           = lhsBinaryExpression.getROperand();

                newExpression += getLeftSideText(lhsBinaryExpression);

                assert (rightSide != null);
            }
            constantValue = ExpressionUtil.computeConstantExpression(getSubexpression(binaryExpression));
        } else {
            constantValue = ExpressionUtil.computeConstantExpression(expression);
        }

        if (constantValue instanceof String) {
            newExpression += '"' + StringUtil.escapeStringCharacters(constantValue.toString()) + '"';
        } else {
            newExpression += String.valueOf(constantValue);
        }
        JSElementFactory.replaceExpression(expression, newExpression);
    }

    private static String getLeftSideText(JSBinaryExpression binaryExpression) {
        return binaryExpression.getLOperand().getText() +
               BinaryOperatorUtils.getOperatorText(binaryExpression.getOperationSign());
    }

    /**
     * Returns the smallest subexpression (if precendence allows it). For instance:
     * variable + 2 + 3 normally gets evaluated left to right -> (variable + 2)
     * + 3 this method returns the right most legal subexpression -> 2 + 3
     * @param expression the expression to analyse
     * @return the found common sub-expression if found, or <tt>null</tt> otherwise.
     */
    @Nullable
    private static JSBinaryExpression getSubexpression(JSBinaryExpression expression) {
        final JSExpression       rhs               = expression.getROperand();
        final IElementType       sign              = expression.getOperationSign();
        final int                parentPrecendence = ParenthesesUtils.getPrecendence(expression);

        if (rhs == null) {
            return null;
        }

        final JSExpression lhs = expression.getLOperand();

        if (!(lhs instanceof JSBinaryExpression)) {
            return expression;
        }

        final JSBinaryExpression lhsBinaryExpression = (JSBinaryExpression) lhs;
        final int                childPrecendence    = ParenthesesUtils.getPrecendence(lhsBinaryExpression);
        final JSExpression       leftSide            = lhsBinaryExpression.getROperand();

        if (leftSide == null) {
            return null;
        }

        if (parentPrecendence > childPrecendence) {
            return null;
        }

        try {
            final String  subExpressionText = leftSide.getText() + BinaryOperatorUtils.getOperatorText(sign) +
                                              rhs.getText();
            final JSExpression subExpression     = JSChangeUtil.createExpressionFromText(expression.getProject(),
                                                                                    subExpressionText,
                                                                                    JSUtils.getDialect(expression.getContainingFile())                                                                                    );

            return (JSBinaryExpression) subExpression;
        } catch (Throwable ignore) {
            return null;
        }
    }

    private static class ConstantSubexpressionPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@NotNull PsiElement element) {
            if (!(element instanceof JSElement ||
                  element.getPrevSibling() instanceof JSElement)) {
                return false;
            }

            PsiElement parent = element.getParent();

            if (!(parent instanceof JSBinaryExpression)) {
                if (element instanceof JSBinaryExpression &&
                    ((JSBinaryExpression) element).getLOperand() instanceof JSBinaryExpression) {
                    parent = element;
                } else {
                    return false;
                }
            }
            final JSBinaryExpression binaryExpression = (JSBinaryExpression) parent;

            final JSBinaryExpression subexpression = getSubexpression(binaryExpression);
            if (subexpression == null) {
                return false;
            }
            if (binaryExpression.equals(subexpression) &&
                !isPartOfConstantExpression(binaryExpression)) {
                // handled by JSConstantExpressionIntention
                return false;
            }
            if (!ExpressionUtil.isConstantExpression(subexpression)) {
                return false;
            }

            return (ExpressionUtil.computeConstantExpression(subexpression) != null);
        }

        private static boolean isPartOfConstantExpression(JSBinaryExpression binaryExpression) {
            final PsiElement containingElement = binaryExpression.getParent();

            return (containingElement instanceof JSExpression &&
                    ExpressionUtil.isConstantExpression((JSExpression) containingElement));
        }
    }
}