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
    ignoreId = "JSReplaceShiftWithMultiplyIntention",
    categories = {"JavaScript", "Shift"},
    fileExtensions = "js"
)
public class JSReplaceShiftWithMultiplyIntention extends JSMutablyNamedIntention {
    @Nonnull
    @Override
    protected LocalizeValue getBasicText() {
        return JSIntentionLocalize.numberReplaceShiftWithMultiply();
    }

    @Override
    @RequiredReadAction
    protected LocalizeValue getTextForElement(PsiElement element) {
        final IElementType tokenType = ((JSBinaryExpression)element).getOperationSign();
        final String operatorString;

        if (element instanceof JSAssignmentExpression) {
            operatorString = JSTokenTypes.LTLTEQ.equals(tokenType) ? "*=" : "/=";
        }
        else {
            operatorString = JSTokenTypes.LTLT.equals(tokenType) ? "*" : "/";
        }

        return JSIntentionLocalize.numberReplaceShiftWithMultiplyMessage(BinaryOperatorUtils.getOperatorText(tokenType), operatorString);
    }

    @Override
    @Nonnull
    public JSElementPredicate getElementPredicate() {
        return new ShiftByLiteralPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        if (element instanceof JSAssignmentExpression assignmentExpression) {
            this.replaceShiftAssignWithMultiplyOrDivideAssign(assignmentExpression);
        }
        else {
            assert (element instanceof JSBinaryExpression);
            this.replaceShiftWithMultiplyOrDivide((JSBinaryExpression)element);
        }
    }

    @RequiredReadAction
    private void replaceShiftAssignWithMultiplyOrDivideAssign(JSAssignmentExpression exp) throws IncorrectOperationException {
        final JSExpression lhs = exp.getLOperand();
        final JSExpression rhs = exp.getROperand();
        final IElementType tokenType = exp.getOperationSign();
        final String assignString = JSTokenTypes.LTLTEQ.equals(tokenType) ? "*=" : "/=";

        final String expString = lhs.getText() + assignString + ShiftUtils.getExpBase2(rhs);

        JSElementFactory.replaceExpression(exp, expString);
    }

    @RequiredReadAction
    private void replaceShiftWithMultiplyOrDivide(JSBinaryExpression exp) throws IncorrectOperationException {
        final JSExpression lhs = exp.getLOperand();
        final JSExpression rhs = exp.getROperand();
        final IElementType tokenType = exp.getOperationSign();
        final String operatorString = JSTokenTypes.LTLT.equals(tokenType) ? "*" : "/";
        final String lhsText = ParenthesesUtils.getParenthesized(lhs, ParenthesesUtils.MULTIPLICATIVE_PRECENDENCE);
        String expString = lhsText + operatorString + ShiftUtils.getExpBase2(rhs);
        final JSElement parent = (JSElement)exp.getParent();

        if (parent != null && parent instanceof JSExpression parentExpression && !(parent instanceof JSParenthesizedExpression)
            && ParenthesesUtils.getPrecendence(parentExpression) < ParenthesesUtils.MULTIPLICATIVE_PRECENDENCE) {
            expString = '(' + expString + ')';
        }
        JSElementFactory.replaceExpression(exp, expString);
    }

    private static class ShiftByLiteralPredicate implements JSElementPredicate {
        @Override
        @RequiredReadAction
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            return element instanceof JSAssignmentExpression assignmentExpression
                ? this.isAssignmentShiftByLiteral(assignmentExpression)
                : element instanceof JSBinaryExpression binaryExpression && this.isBinaryShiftByLiteral(binaryExpression);
        }

        @RequiredReadAction
        private boolean isAssignmentShiftByLiteral(JSAssignmentExpression expression) {
            final IElementType tokenType = expression.getOperationSign();

            if (tokenType == null || !(JSTokenTypes.LTLTEQ.equals(tokenType) || JSTokenTypes.GTGTEQ.equals(tokenType))) {
                return false;
            }

            final JSExpression rhs = expression.getROperand();

            return rhs != null && ShiftUtils.isIntLiteral(rhs);
        }

        @RequiredReadAction
        private boolean isBinaryShiftByLiteral(JSBinaryExpression expression) {
            final IElementType tokenType = expression.getOperationSign();

            return (JSTokenTypes.LTLT.equals(tokenType) || JSTokenTypes.GTGT.equals(tokenType))
                && ShiftUtils.isIntLiteral(expression.getROperand());
        }
    }
}
