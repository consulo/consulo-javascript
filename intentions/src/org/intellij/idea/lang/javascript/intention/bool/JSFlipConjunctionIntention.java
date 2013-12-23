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

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSMutablyNamedIntention;
import org.intellij.idea.lang.javascript.psiutil.BinaryOperatorUtils;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;
import org.jetbrains.annotations.NotNull;

import com.intellij.lang.javascript.psi.JSBinaryExpression;
import com.intellij.lang.javascript.psi.JSElement;
import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.IncorrectOperationException;

public class JSFlipConjunctionIntention extends JSMutablyNamedIntention {
    @Override
	protected String getTextForElement(PsiElement element) {
        final JSBinaryExpression binaryExpression = (JSBinaryExpression) element;
        final IElementType       sign             = binaryExpression.getOperationSign();

        return this.getText(BinaryOperatorUtils.getOperatorText(sign));
    }

    @Override
	@NotNull public JSElementPredicate getElementPredicate() {
        return new ConjunctionPredicate();
    }

    @Override
	public void processIntention(@NotNull PsiElement element) throws IncorrectOperationException {
        final JSBinaryExpression binaryExpression = (JSBinaryExpression) element;
        JSExpression             exp              = binaryExpression;

        final IElementType sign   = binaryExpression.getOperationSign();
        JSElement          parent = (JSElement) exp.getParent();

        while (isConjunctionExpression(parent, sign)) {
            exp = (JSExpression) parent;
            assert (exp != null);
            parent = (JSElement) exp.getParent();
        }
        JSElementFactory.replaceExpression(exp, this.flipExpression(exp, sign));
    }

    private String flipExpression(JSExpression exp,
                                  IElementType conjunctionType) {
        if (isConjunctionExpression(exp, conjunctionType)) {
            final JSBinaryExpression andExpression = (JSBinaryExpression) exp;

            return this.flipExpression(andExpression.getROperand(), conjunctionType) + ' ' +
                   BinaryOperatorUtils.getOperatorText(conjunctionType) + ' ' +
                   this.flipExpression(andExpression.getLOperand(), conjunctionType);
        } else{
            return exp.getText();
        }
    }

    private static boolean isConjunctionExpression(JSElement    expression,
                                                   IElementType conjunctionType) {
        if(!(expression instanceof JSBinaryExpression)) {
            return false;
        }

        final JSBinaryExpression binaryExpression = (JSBinaryExpression) expression;

        return binaryExpression.getOperationSign().equals(conjunctionType);
    }
}
