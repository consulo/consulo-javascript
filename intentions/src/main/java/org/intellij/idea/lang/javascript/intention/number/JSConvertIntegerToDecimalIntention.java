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

import javax.annotation.Nonnull;

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.NumberUtil;

import com.intellij.lang.javascript.psi.JSLiteralExpression;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;


public class JSConvertIntegerToDecimalIntention extends JSIntention {
    @Override
	@Nonnull
    public JSElementPredicate getElementPredicate() {
        return new ConvertIntegerToDecimalPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final JSLiteralExpression exp = (JSLiteralExpression) element;

        JSElementFactory.replaceExpression(exp, NumberUtil.getLiteralNumber(exp).toString());
    }

    private static class ConvertIntegerToDecimalPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSLiteralExpression)) {
                return false;
            }

            final String elementText = element.getText();

            return (NumberUtil.isHex  (elementText) ||
                    NumberUtil.isOctal(elementText));
        }
    }
}
