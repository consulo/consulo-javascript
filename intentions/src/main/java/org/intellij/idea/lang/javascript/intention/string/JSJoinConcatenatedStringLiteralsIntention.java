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

import com.intellij.lang.javascript.JSTokenTypes;
import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import consulo.annotation.access.RequiredReadAction;
import consulo.annotation.component.ExtensionImpl;
import consulo.javascript.intention.localize.JSIntentionLocalize;
import consulo.javascript.lang.JavaScriptTokenSets;
import consulo.javascript.psi.JSSimpleLiteralExpression;
import consulo.language.editor.intention.IntentionMetaData;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;
import consulo.localize.LocalizeValue;
import jakarta.annotation.Nonnull;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

@ExtensionImpl
@IntentionMetaData(
    ignoreId = "JSJoinConcatenatedStringLiteralsIntention",
    categories = {"JavaScript", "Other"},
    fileExtensions = "js"
)
public class JSJoinConcatenatedStringLiteralsIntention extends JSIntention {
    @Override
    @Nonnull
    public LocalizeValue getText() {
        return JSIntentionLocalize.stringJoinConcatenatedStringLiterals();
    }

    @Override
    @Nonnull
    protected JSElementPredicate getElementPredicate() {
        return new StringConcatPredicate();
    }

    @Override
    @RequiredReadAction
    public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        JSBinaryExpression expression = (JSBinaryExpression)element;
        JSExpression lhs = expression.getLOperand();
        JSExpression rhs = expression.getROperand();

        assert (lhs instanceof JSLiteralExpression && rhs instanceof JSLiteralExpression);

        JSLiteralExpression leftLiteral = (JSLiteralExpression)lhs;
        JSLiteralExpression rightLiteral = (JSLiteralExpression)rhs;
        String lhsText = lhs.getText();
        String rhsText = rhs.getText();
        String newExpression;

        if (StringUtil.isSimpleQuoteStringLiteral(leftLiteral)
            && StringUtil.isDoubleQuoteStringLiteral(rightLiteral)) {
            rhsText = JSDoubleToSingleQuotedStringIntention.changeQuotes(rhsText);
        }
        else if (StringUtil.isDoubleQuoteStringLiteral(leftLiteral)
            && StringUtil.isSimpleQuoteStringLiteral(rightLiteral)) {
            rhsText = JSSingleToDoubleQuotedStringIntention.changeQuotes(rhsText);
        }

        newExpression = lhsText.substring(0, lhsText.length() - 1) + rhsText.substring(1);
        JSElementFactory.replaceExpression(expression, newExpression);
    }

    private static class StringConcatPredicate implements JSElementPredicate {
        @Override
        @RequiredReadAction
        public boolean satisfiedBy(@Nonnull PsiElement element) {
            return element instanceof JSBinaryExpression expression
                && JSTokenTypes.PLUS.equals(expression.getOperationSign())
                && isApplicableLiteral(expression.getLOperand())
                && isApplicableLiteral(expression.getROperand());
        }

        @RequiredReadAction
        private static boolean isApplicableLiteral(JSExpression lhs) {
            return lhs != null
                && lhs instanceof JSSimpleLiteralExpression simpleLiteralExpression
                && JavaScriptTokenSets.STRING_LITERALS.contains(simpleLiteralExpression.getLiteralElementType());
        }
    }
}
