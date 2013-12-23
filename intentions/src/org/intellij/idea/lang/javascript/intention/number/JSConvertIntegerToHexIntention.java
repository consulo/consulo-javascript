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
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.intellij.idea.lang.javascript.psiutil.NumberUtil;
import org.jetbrains.annotations.NotNull;

import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.psi.PsiElement;
import com.intellij.util.IncorrectOperationException;

public class JSConvertIntegerToHexIntention extends JSIntention {
    @Override
	@NotNull
    public JSElementPredicate getElementPredicate() {
        return new ConvertIntegerToHexPredicate();
    }

    @Override
	public void processIntention(@NotNull PsiElement element) throws IncorrectOperationException {
        final JSLiteralExpression exp = (JSLiteralExpression) element;

        JSElementFactory.replaceExpression(exp, "0x" + NumberUtil.getLiteralNumber(exp).toString(16));
    }

    private static class ConvertIntegerToHexPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@NotNull PsiElement element) {
            if (!(element instanceof JSLiteralExpression)) {
                return false;
            }

            return NumberUtil.isDecimal(element.getText());
        }
    }
}
