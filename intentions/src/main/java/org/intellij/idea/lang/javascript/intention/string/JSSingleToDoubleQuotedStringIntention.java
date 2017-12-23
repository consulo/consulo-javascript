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

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NotNull;

import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;

public class JSSingleToDoubleQuotedStringIntention extends JSIntention {
    @Override
	@NotNull
    protected JSElementPredicate getElementPredicate() {
        return new SingleToDoubleQuotedStringPredicate();
    }

    @Override
	public void processIntention(@NotNull PsiElement element) throws IncorrectOperationException {
        final JSLiteralExpression charLiteral = (JSLiteralExpression) element;

        JSElementFactory.replaceExpression(charLiteral, changeQuotes(charLiteral.getText()));
    }

    static String changeQuotes(String charLiteral) {
        StringBuilder buffer      = new StringBuilder(charLiteral);
        int           simpleIndex = charLiteral.lastIndexOf(StringUtil.SIMPLE_QUOTE, charLiteral.length() - 2);
        int           doubleIndex = charLiteral.lastIndexOf(StringUtil.DOUBLE_QUOTE);

        while (simpleIndex > 0 || doubleIndex >= 0) {
            if (simpleIndex > doubleIndex) {
                if (charLiteral.charAt(simpleIndex - 1) == StringUtil.BACKSLASH) {
                    buffer.deleteCharAt(simpleIndex - 1);
                }
                simpleIndex = charLiteral.lastIndexOf(StringUtil.SIMPLE_QUOTE, simpleIndex - 2);
            } else {
                if (charLiteral.charAt(doubleIndex - 1) != StringUtil.BACKSLASH) {
                    buffer.insert(doubleIndex, StringUtil.BACKSLASH);
                }
                doubleIndex = charLiteral.lastIndexOf(StringUtil.DOUBLE_QUOTE, doubleIndex - 1);
            }
        }
        buffer.setCharAt(0,                   StringUtil.DOUBLE_QUOTE);
        buffer.setCharAt(buffer.length() - 1, StringUtil.DOUBLE_QUOTE);

        return buffer.toString();
    }

    private static class SingleToDoubleQuotedStringPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@NotNull PsiElement element) {
            if (!(element instanceof JSLiteralExpression)) {
                return false;
            }

            final JSLiteralExpression expression = (JSLiteralExpression) element;

            return StringUtil.isSimpleQuoteStringLiteral(expression);
        }
    }
}
