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
package org.intellij.idea.lang.javascript.intention.number;

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NotNull;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSAssignmentExpression;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSParenthesizedExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;

public class JSReplaceMultiplyWithShiftIntention extends JSMutablyNamedIntention {
    @Override
	protected String getTextForElement(PsiElement element) {
        final IElementType  tokenType = ((JSBinaryExpression) element).getOperationSign();
        final String        operatorString;

        if (element instanceof JSAssignmentExpression) {
            operatorString = (tokenType.equals(JSTokenTypes.MULTEQ) ? "<<=" : ">>=");
        } else {
            operatorString = (tokenType.equals(JSTokenTypes.MULT)   ? "<<"  : ">>");
        }

        return this.getText(BinaryOperatorUtils.getOperatorText(tokenType), operatorString);
    }

    @Override
	@NotNull
    public JSElementPredicate getElementPredicate() {
        return new MultiplyByPowerOfTwoPredicate();
    }

    @Override
	public void processIntention(@NotNull PsiElement element)
            throws IncorrectOperationException {
        if (element instanceof JSAssignmentExpression) {
            this.replaceMultiplyOrDivideAssignWithShiftAssign((JSAssignmentExpression) element);
        } else {
            this.replaceMultiplyOrDivideWithShift((JSBinaryExpression) element);
        }
    }

    private void replaceMultiplyOrDivideAssignWithShiftAssign(JSAssignmentExpression exp)
            throws IncorrectOperationException {
        final JSExpression lhs          = exp.getLOperand();
        final JSExpression rhs          = exp.getROperand();
        final IElementType tokenType    = exp.getOperationSign();
        final String       assignString = (tokenType.equals(JSTokenTypes.MULTEQ) ? "<<=" : ">>=");
        final String       expString    = lhs.getText() + assignString +
                                          ShiftUtils.getLogBase2(rhs);

        JSElementFactory.replaceExpression(exp, expString);
    }

    private void replaceMultiplyOrDivideWithShift(JSBinaryExpression exp)
            throws IncorrectOperationException {
        JSExpression       lhs            = exp.getLOperand();
        JSExpression       rhs            = exp.getROperand();
        final IElementType tokenType      = exp.getOperationSign();
        final String       operatorString = (tokenType.equals(JSTokenTypes.MULT) ? "<<" : ">>");

        if (ShiftUtils.isPowerOfTwo(lhs) && tokenType.equals(JSTokenTypes.MULT)) {
            JSExpression  swap = lhs;

            lhs = rhs;
            rhs = swap;
        }

        final String     lhsText   = ParenthesesUtils.getParenthesized(lhs, ParenthesesUtils.SHIFT_PRECENDENCE);
        String           expString = lhsText + operatorString + ShiftUtils.getLogBase2(rhs);
        final  JSElement parent    = (JSElement) exp.getParent();

        if (parent != null && parent instanceof JSExpression) {
            if (!(parent instanceof JSParenthesizedExpression)  &&
                ParenthesesUtils.getPrecendence((JSExpression) parent) < ParenthesesUtils.SHIFT_PRECENDENCE) {
                expString = '(' + expString + ')';
            }
        }
        JSElementFactory.replaceExpression(exp, expString);
    }

    private static class MultiplyByPowerOfTwoPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@NotNull PsiElement element) {
            if (element instanceof JSAssignmentExpression) {
                return isMultiplyByPowerOfTwo((JSAssignmentExpression) element);
            } else if (element instanceof JSBinaryExpression) {
                return isMultiplyByPowerOfTwo((JSBinaryExpression) element);
            } else {
                return false;
            }
        }

        private static boolean isMultiplyByPowerOfTwo(JSAssignmentExpression expression) {
            final IElementType operator = expression.getOperationSign();

            if (operator == null || !(operator.equals(JSTokenTypes.MULTEQ) || operator.equals(JSTokenTypes.DIVEQ))) {
                return false;
            }

            final JSExpression rightExpression = expression.getROperand();

            if (rightExpression == null) {
                return false;
            }

            return ShiftUtils.isPowerOfTwo(rightExpression);
        }

        private static boolean isMultiplyByPowerOfTwo(JSBinaryExpression expression) {
            final IElementType operator = expression.getOperationSign();

            if (operator == null || !(operator.equals(JSTokenTypes.MULT) || operator.equals(JSTokenTypes.DIV))) {
                return false;
            }

            final JSExpression leftOperand  = expression.getLOperand();
            final JSExpression rightOperand = expression.getROperand();

            if (leftOperand == null || rightOperand == null) {
                return false;
            }
            return (ShiftUtils.isPowerOfTwo(leftOperand) || ShiftUtils.isPowerOfTwo(rightOperand));
        }
    }
}
