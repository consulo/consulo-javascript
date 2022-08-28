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
package org.intellij.idea.lang.javascript.intention.string;

import javax.annotation.Nonnull;

import consulo.language.ast.IElementType;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.annotation.access.RequiredReadAction;
import consulo.javascript.lang.JavaScriptTokenSets;

public class JSJoinConcatenatedStringLiteralsIntention extends JSIntention {
    @Override
	@Nonnull
    protected JSElementPredicate getElementPredicate() {
        return new StringConcatPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element)
            throws IncorrectOperationException {
        final JSBinaryExpression expression = (JSBinaryExpression) element;
        final JSExpression       lhs        = expression.getLOperand();
        final JSExpression       rhs        = expression.getROperand();

        assert (lhs instanceof JSLiteralExpression && rhs instanceof JSLiteralExpression);

        final JSLiteralExpression  leftLiteral  = (JSLiteralExpression) lhs;
        final JSLiteralExpression  rightLiteral = (JSLiteralExpression) rhs;
        String                     lhsText      = lhs.getText();
        String                     rhsText      = rhs.getText();
        final String               newExpression;

        if (StringUtil.isSimpleQuoteStringLiteral(leftLiteral) &&
            StringUtil.isDoubleQuoteStringLiteral(rightLiteral)) {
            rhsText = JSDoubleToSingleQuotedStringIntention.changeQuotes(rhsText);
        } else if (StringUtil.isDoubleQuoteStringLiteral(leftLiteral) &&
                   StringUtil.isSimpleQuoteStringLiteral(rightLiteral)) {
            rhsText = JSSingleToDoubleQuotedStringIntention.changeQuotes(rhsText);
        }

        newExpression = lhsText.substring(0, lhsText.length() - 1) + rhsText.substring(1);
        JSElementFactory.replaceExpression(expression, newExpression);
    }

    private static class StringConcatPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSBinaryExpression)) {
                return false;
            }

            final JSBinaryExpression expression = (JSBinaryExpression) element;
            final IElementType sign       = expression.getOperationSign();

            if (!sign.equals(JSTokenTypes.PLUS)) {
                return false;
            }
            final JSExpression lhs = expression.getLOperand();
            final JSExpression rhs = expression.getROperand();

            if (!isApplicableLiteral(lhs)) {
                return false;
            }
            if (!isApplicableLiteral(rhs)) {
                return false;
            }

            return true;
        }

		@RequiredReadAction
        private static boolean isApplicableLiteral(JSExpression lhs) {
            return lhs != null &&
                   lhs instanceof JSSimpleLiteralExpression &&
                    JavaScriptTokenSets.STRING_LITERALS.contains(((JSSimpleLiteralExpression) lhs).getLiteralElementType());
        }
    }
}
