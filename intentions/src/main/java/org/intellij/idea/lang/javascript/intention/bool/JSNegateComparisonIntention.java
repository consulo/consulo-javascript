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

import javax.annotation.Nonnull;

import consulo.language.ast.IElementType;
import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.ComparisonUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSExpression;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;

public class JSNegateComparisonIntention extends JSMutablyNamedIntention {
    @Override
	public String getTextForElement(PsiElement element) {
        final JSBinaryExpression expression          = (JSBinaryExpression) element;
        String                   operatorText        = "";
        String                   negatedOperatorText = "";

        if (expression != null) {
            final IElementType sign = expression.getOperationSign();

            operatorText        = ComparisonUtils.getOperatorText(sign);
            negatedOperatorText = ComparisonUtils.getNegatedOperatorText(sign);
        }

        if (operatorText.equals(negatedOperatorText)) {
            return this.getSuffixedDisplayName("equals", operatorText);
        } else {
            return this.getSuffixedDisplayName("not-equals", operatorText, negatedOperatorText);
        }
    }

    @Override
	@Nonnull
    public JSElementPredicate getElementPredicate() {
        return new ComparisonPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final JSBinaryExpression exp             = (JSBinaryExpression) element;
        final JSExpression       lhs             = exp.getLOperand();
        final JSExpression       rhs             = exp.getROperand();
        final IElementType sign            = exp.getOperationSign();
        final String             negatedOperator = ComparisonUtils.getNegatedOperatorText(sign);
        final String             lhsText         = lhs.getText();

        assert (rhs != null);

        JSElementFactory.replaceExpressionWithNegatedExpressionString(exp, lhsText +
                                                                           negatedOperator +
                                                                           rhs.getText());
    }
}
