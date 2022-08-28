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
package org.intellij.idea.lang.javascript.intention.constant;

import javax.annotation.Nonnull;

import org.intellij.idea.lang.javascript.intention.JSElementPredicate;
import org.intellij.idea.lang.javascript.intention.JSIntention;
import org.intellij.idea.lang.javascript.psiutil.ErrorUtil;
import org.intellij.idea.lang.javascript.psiutil.ExpressionUtil;
import org.intellij.idea.lang.javascript.psiutil.JSElementFactory;

import com.intellij.lang.javascript.psi.JSExpression;
import com.intellij.lang.javascript.psi.JSLiteralExpression;
import com.intellij.lang.javascript.psi.JSReferenceExpression;
import com.intellij.lang.javascript.psi.JSCallExpression;
import consulo.util.lang.StringUtil;
import consulo.language.psi.PsiElement;
import consulo.language.util.IncorrectOperationException;

public class JSConstantExpressionIntention extends JSIntention {
    @Override
	@Nonnull
    protected JSElementPredicate getElementPredicate() {
        return new ConstantExpressionPredicate();
    }

    @Override
	public void processIntention(@Nonnull PsiElement element) throws IncorrectOperationException {
        final JSExpression  expression = (JSExpression) element;
        final Object        value      = ExpressionUtil.computeConstantExpression(expression);
        final String        newExpression;

        if (value instanceof String) {
            newExpression = '"' + StringUtil.escapeStringCharacters((String) value) + '"';
        } else {
            newExpression = String.valueOf(value);
        }
        JSElementFactory.replaceExpression(expression, newExpression);
    }

    private static class ConstantExpressionPredicate implements JSElementPredicate {
        @Override
		public boolean satisfiedBy(@Nonnull PsiElement element) {
            if (!(element instanceof JSExpression)) {
                return false;
            }
            if (ErrorUtil.containsError(element)) {
                return false;
            }
            final JSExpression expression = (JSExpression) element;

            if (element instanceof JSLiteralExpression ||
                ( element instanceof JSReferenceExpression &&
                  ((JSReferenceExpression)element).getQualifier() != null
                ) ||
                expression instanceof JSCallExpression
               ) {
                return false;
            }
            if (!ExpressionUtil.isConstantExpression(expression)) {
                return false;
            }

            if (ExpressionUtil.computeConstantExpression(expression) == null) {
                return false;
            }

            final PsiElement parent = element.getParent();

            if (!(parent instanceof JSExpression)) {
                return true;
            }
            return (!ExpressionUtil.isConstantExpression((JSExpression) parent));
        }
    }
}
