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
package org.intellij.idea.lang.javascript.intention.bool;

import consulo.language.psi.PsiElement;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.BoolUtils;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import javax.annotation.Nonnull;

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.language.ast.IElementType;
import consulo.language.util.IncorrectOperationException;

public class JSRemoveBooleanEqualityIntention extends JSMutablyNamedIntention {
    @Override
	protected String getTextForElement(PsiElement element) {
        final JSBinaryExpression binaryExpression = (JSBinaryExpression) element;

        return this.getText(BinaryOperatorUtils.getOperatorText(binaryExpression.getOperationSign()));
    }

    @Override
	@Nonnull
    public JSElementPredicate getElementPredicate() {
        return new BooleanLiteralEqualityPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element)
            throws IncorrectOperationException {
        final JSBinaryExpression exp      = (JSBinaryExpression) element;
        final boolean            isEquals = exp.getOperationSign().equals(JSTokenTypes.EQEQ);
        final JSExpression       lhs      = exp.getLOperand();
        final JSExpression       rhs      = exp.getROperand();

        assert (lhs != null);
        assert (rhs != null);

        final String lhsText = lhs.getText();
        final String rhsText = rhs.getText();

        if (BoolUtils.TRUE.equals(lhsText)) {
            if (isEquals) {
                JSElementFactory.replaceExpression(exp, rhsText);
            } else{
                JSElementFactory.replaceExpressionWithNegatedExpression(rhs, exp);
            }
        } else if (BoolUtils.FALSE.equals(lhsText)) {
            if (isEquals) {
                JSElementFactory.replaceExpressionWithNegatedExpression(rhs, exp);
            } else {
                JSElementFactory.replaceExpression(exp, rhsText);
            }
        } else if (BoolUtils.TRUE.equals(rhsText)) {
            if (isEquals) {
                JSElementFactory.replaceExpression(exp, lhsText);
            } else {
                JSElementFactory.replaceExpressionWithNegatedExpression(lhs, exp);
            }
        } else {
            if (isEquals) {
                JSElementFactory.replaceExpressionWithNegatedExpression(lhs, exp);
            } else {
                JSElementFactory.replaceExpression(exp, lhsText);
            }
        }
    }

    private static class BooleanLiteralEqualityPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSBinaryExpression)) {
                return false;
            }
            if (ErrorUtil.containsError(element)) {
                return false;
            }

            final JSBinaryExpression expression = (JSBinaryExpression) element;
            final IElementType       sign       = expression.getOperationSign();

            if (!(sign.equals(JSTokenTypes.EQEQ) || sign.equals(JSTokenTypes.NE))) {
                return false;
            }

            final JSExpression lhs = expression.getLOperand();
            final JSExpression rhs = expression.getROperand();

            return (lhs != null && rhs != null &&
                    (BoolUtils.isBooleanLiteral(lhs) || BoolUtils.isBooleanLiteral(rhs)));
        }
    }
}
