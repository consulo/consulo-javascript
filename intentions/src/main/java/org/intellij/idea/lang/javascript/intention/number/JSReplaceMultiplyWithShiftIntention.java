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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.*;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.language.ast.IElementType;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.ParenthesesUtils;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSReplaceMultiplyWithShiftIntention",
    categories = {"JavaScript", "Shift"},
    fileExtensions = "js"
)
public class JSReplaceMultiplyWithShiftIntention extends JSMutablyNamedIntention {
    @Nonnull
    @Override
    protected LocalizeValue getBasicText() {
        return JSIntentionLocalize.numberReplaceMultiplyWithShift();
    }

    @Override
    @RequiredReadAction
    protected LocalizeValue getTextForElement(PsiElement element) {
        IElementType tokenType = ((JSBinaryExpression)element).getOperationSign();
        String operatorString;

        if (element instanceof JSAssignmentExpression) {
            operatorString = JSTokenTypes.MULTEQ.equals(tokenType) ? "<<=" : ">>=";
        }
        else {
            operatorString = JSTokenTypes.MULT.equals(tokenType) ? "<<" : ">>";
        }

        return JSIntentionLocalize.numberReplaceMultiplyWithShiftMessage(BinaryOperatorUtils.getOperatorText(tokenType), operatorString);
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new MultiplyByPowerOfTwoPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        if (element instanceof JSAssignmentExpression assignmentExpression) {
            this.replaceMultiplyOrDivideAssignWithShiftAssign(assignmentExpression);
        }
        else {
            this.replaceMultiplyOrDivideWithShift((JSBinaryExpression)element);
        }
    }

    @RequiredReadAction
    private void replaceMultiplyOrDivideAssignWithShiftAssign(JSAssignmentExpression exp) throws IncorrectOperationException {
        JSExpression lhs = exp.getLOperand();
        JSExpression rhs = exp.getROperand();
        IElementType tokenType = exp.getOperationSign();
        String assignString = JSTokenTypes.MULTEQ.equals(tokenType) ? "<<=" : ">>=";
        String expString = lhs.getText() + assignString + ShiftUtils.getLogBase2(rhs);

        JSElementFactory.replaceExpression(exp, expString);
    }

    @RequiredReadAction
    private void replaceMultiplyOrDivideWithShift(JSBinaryExpression exp) throws IncorrectOperationException {
        JSExpression lhs = exp.getLOperand();
        JSExpression rhs = exp.getROperand();
        IElementType tokenType = exp.getOperationSign();
        String operatorString = JSTokenTypes.MULT.equals(tokenType) ? "<<" : ">>";

        if (ShiftUtils.isPowerOfTwo(lhs) && JSTokenTypes.MULT.equals(tokenType)) {
            JSExpression swap = lhs;

            lhs = rhs;
            rhs = swap;
        }

        String lhsText = ParenthesesUtils.getParenthesized(lhs, ParenthesesUtils.SHIFT_PRECENDENCE);
        String expString = lhsText + operatorString + ShiftUtils.getLogBase2(rhs);
        JSElement parent = (JSElement)exp.getParent();

        if (parent != null && parent instanceof JSExpression parentExpression && !(parent instanceof JSParenthesizedExpression)
            && ParenthesesUtils.getPrecendence(parentExpression) < ParenthesesUtils.SHIFT_PRECENDENCE) {
            expString = '(' + expString + ')';
        }
        JSElementFactory.replaceExpression(exp, expString);
    }

    private static class MultiplyByPowerOfTwoPredicate implements JSElementPredicate {
        @Override
        @RequiredReadAction
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            return element instanceof JSAssignmentExpression assignmentExpression
                ? isMultiplyByPowerOfTwo(assignmentExpression)
                : element instanceof JSBinaryExpression binaryExpression && isMultiplyByPowerOfTwo(binaryExpression);
        }

        @RequiredReadAction
        private static boolean isMultiplyByPowerOfTwo(JSAssignmentExpression expression) {
            IElementType operator = expression.getOperationSign();

            if (operator == null || !(operator.equals(JSTokenTypes.MULTEQ) || operator.equals(JSTokenTypes.DIVEQ))) {
                return false;
            }

            JSExpression rightExpression = expression.getROperand();

            return rightExpression != null && ShiftUtils.isPowerOfTwo(rightExpression);
        }

        @RequiredReadAction
        private static boolean isMultiplyByPowerOfTwo(JSBinaryExpression expression) {
            IElementType operator = expression.getOperationSign();

            if (operator == null || !(JSTokenTypes.MULT.equals(operator) || JSTokenTypes.DIV.equals(operator))) {
                return false;
            }

            JSExpression leftOperand = expression.getLOperand();
            JSExpression rightOperand = expression.getROperand();

            return leftOperand != null && rightOperand != null
                && (ShiftUtils.isPowerOfTwo(leftOperand) || ShiftUtils.isPowerOfTwo(rightOperand));
        }
    }
}
