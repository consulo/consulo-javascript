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

public class JSReplaceShiftWithMultiplyIntention extends JSMutablyNamedIntention {
    @Override
	protected String getTextForElement(PsiElement element) {
        final IElementType  tokenType = ((JSBinaryExpression) element).getOperationSign();
        final String        operatorString;

        if (element instanceof JSAssignmentExpression) {
            operatorString = (tokenType.equals(JSTokenTypes.LTLTEQ) ? "*=" : "/=");
        } else {
            operatorString = (tokenType.equals(JSTokenTypes.LTLT)   ? "*"  : "/");
        }

        return this.getText(BinaryOperatorUtils.getOperatorText(tokenType), operatorString);
    }

    @Override
	@NotNull
    public JSElementPredicate getElementPredicate() {
        return new ShiftByLiteralPredicate();
    }

    @Override
	public void processIntention(@NotNull PsiElement element) throws IncorrectOperationException {
        if (element instanceof JSAssignmentExpression) {
            this.replaceShiftAssignWithMultiplyOrDivideAssign((JSAssignmentExpression) element);
        } else {
            assert(element instanceof JSBinaryExpression);
            this.replaceShiftWithMultiplyOrDivide((JSBinaryExpression) element);
        }
    }

    private void replaceShiftAssignWithMultiplyOrDivideAssign(JSAssignmentExpression exp)
            throws IncorrectOperationException {
        final JSExpression  lhs          = exp.getLOperand();
        final JSExpression  rhs          = exp.getROperand();
        final IElementType  tokenType    = exp.getOperationSign();
        final String        assignString = ((tokenType.equals(JSTokenTypes.LTLTEQ)) ? "*=" : "/=");

        final String expString = lhs.getText() + assignString + ShiftUtils.getExpBase2(rhs);

        JSElementFactory.replaceExpression(exp, expString);
    }

    private void replaceShiftWithMultiplyOrDivide(JSBinaryExpression exp)
            throws IncorrectOperationException {
        final JSExpression  lhs            = exp.getLOperand();
        final JSExpression  rhs            = exp.getROperand();
        final IElementType  tokenType      = exp.getOperationSign();
        final String        operatorString = ((tokenType.equals(JSTokenTypes.LTLT)) ? "*" : "/");
        final String        lhsText        = ParenthesesUtils.getParenthesized(lhs, ParenthesesUtils.MULTIPLICATIVE_PRECENDENCE);
        String              expString      = lhsText + operatorString + ShiftUtils.getExpBase2(rhs);
        final JSElement     parent         = (JSElement) exp.getParent();

        if (parent != null && parent instanceof JSExpression) {
            if (!(parent instanceof JSParenthesizedExpression)  &&
                ParenthesesUtils.getPrecendence((JSExpression) parent) < ParenthesesUtils.MULTIPLICATIVE_PRECENDENCE) {
                expString = '(' + expString + ')';
            }
        }
        JSElementFactory.replaceExpression(exp, expString);
    }

    private static class ShiftByLiteralPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@NotNull PsiElement element) {
            if (element instanceof JSAssignmentExpression) {
                return this.isAssignmentShiftByLiteral((JSAssignmentExpression) element);
            } else if (element instanceof JSBinaryExpression) {
                return this.isBinaryShiftByLiteral((JSBinaryExpression) element);
            } else {
                return false;
            }
        }

        private boolean isAssignmentShiftByLiteral(JSAssignmentExpression expression) {
            final IElementType tokenType = expression.getOperationSign();

            if (tokenType == null ||
                !(tokenType.equals(JSTokenTypes.LTLTEQ) ||
                  tokenType.equals(JSTokenTypes.GTGTEQ)
                )
               ) {
                return false;
            }

            final JSExpression rhs = expression.getROperand();

            if (rhs == null) {
                return false;
            }
            return ShiftUtils.isIntLiteral(rhs);
        }

        private boolean isBinaryShiftByLiteral(JSBinaryExpression expression) {
            final IElementType tokenType = expression.getOperationSign();

            if (!(tokenType.equals(JSTokenTypes.LTLT) ||
                  tokenType.equals(JSTokenTypes.GTGT))) {
                return false;
            }

            return ShiftUtils.isIntLiteral(expression.getROperand());
        }
    }
}
